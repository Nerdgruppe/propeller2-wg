using System.Buffers.Binary;
using System.Net;
using System.Net.Sockets;
using System.Threading.Channels;

namespace Scopecreep.Relay;

/// <summary>Maintains one scope connection while local video/control clients come and go.</summary>
internal sealed class RemoteRelay
{
    private const string ScopeAddress = "192.168.178.115";
    private const int Port = 8888;
    private const int HeaderSize = 8;
    private const int EventSize = 12;
    private const int MaxPayloadSize = 16 * 1024 * 1024;
    private static readonly TimeSpan ConnectTimeout = TimeSpan.FromSeconds(5);
    private static readonly TimeSpan WriteTimeout = TimeSpan.FromSeconds(2);
    private static readonly TimeSpan ReconnectDelay = TimeSpan.FromSeconds(5);
    private readonly object stateLock = new();
    private ScopeConnection? scope;
    private ClientConnection? client;
    private Task clientTask = Task.CompletedTask;

    /// <summary>Runs independent upstream and local accept loops, supervising their shutdown.</summary>
    public async Task RunAsync(CancellationToken cancellationToken)
    {
        using var stopping = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken);
        using var listener = new TcpListener(IPAddress.Loopback, Port);
        listener.Start();
        Console.Error.WriteLine($"Listening on 127.0.0.1:{Port}; scope is {ScopeAddress}:{Port}");
        var upstream = MaintainScopeAsync(stopping.Token);
        var accepting = AcceptClientsAsync(listener, stopping.Token);
        try
        {
            await Task.WhenAny(upstream, accepting);
        }
        finally
        {
            stopping.Cancel();
            listener.Stop();
            ScopeConnection? closingScope;
            ClientConnection? closingClient;
            Task closingTask;
            lock (stateLock)
            {
                closingScope = scope;
                closingClient = client;
                closingTask = clientTask;
            }
            closingClient?.Close();
            closingScope?.Close();
            await Task.WhenAll(upstream, accepting, closingTask);
        }
    }

    /// <summary>Reconnects only after an upstream failure; local client lifetimes do not affect this loop.</summary>
    private async Task MaintainScopeAsync(CancellationToken cancellationToken)
    {
        while (true)
        {
            cancellationToken.ThrowIfCancellationRequested();
            using var connection = new ScopeConnection(cancellationToken);
            try
            {
                using var connecting = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken);
                connecting.CancelAfter(ConnectTimeout);
                await connection.Socket.ConnectAsync(ScopeAddress, Port, connecting.Token);
                lock (stateLock)
                {
                    cancellationToken.ThrowIfCancellationRequested();
                    scope = connection;
                }
                Console.Error.WriteLine("Scope connected; approve remote access on the scope if prompted");
                await ReadVideoAsync(connection);
            }
            catch (Exception error) when (error is IOException or InvalidDataException or SocketException or OperationCanceledException)
            {
                if (!cancellationToken.IsCancellationRequested)
                    Console.Error.WriteLine($"Scope connection ended: {error.Message}; retrying in 5 seconds");
            }
            finally
            {
                ClientConnection? closingClient;
                Task closingTask;
                lock (stateLock)
                {
                    if (ReferenceEquals(scope, connection)) scope = null;
                    closingClient = client?.Scope == connection ? client : null;
                    closingTask = closingClient is null ? Task.CompletedTask : clientTask;
                }
                connection.Close();
                closingClient?.Close();
                await closingTask;
            }
            await Task.Delay(ReconnectDelay, cancellationToken);
        }
    }

    /// <summary>Accepts one local client and rejects additional clients or clients without an upstream connection.</summary>
    private async Task AcceptClientsAsync(TcpListener listener, CancellationToken cancellationToken)
    {
        while (true)
        {
            TcpClient socket;
            try
            {
                socket = await listener.AcceptTcpClientAsync(cancellationToken);
            }
            catch (Exception error) when (cancellationToken.IsCancellationRequested &&
                error is OperationCanceledException or ObjectDisposedException or SocketException)
            {
                return;
            }
            socket.NoDelay = true;
            var accepted = false;
            lock (stateLock)
            {
                if (!cancellationToken.IsCancellationRequested && client is null &&
                    scope is not null && !scope.Token.IsCancellationRequested)
                {
                    var connection = new ClientConnection(socket, scope, cancellationToken);
                    client = connection;
                    // Start after publication; the lock protects clientTask against a concurrent scope disconnect.
                    clientTask = Task.Run(() => HandleClientAsync(connection));
                    accepted = true;
                }
            }
            if (!accepted)
            {
                socket.Dispose();
                Console.Error.WriteLine("Local client rejected: another client is active or scope is disconnected");
            }
        }
    }

    /// <summary>Observes both client pumps and releases the local slot without disconnecting the scope.</summary>
    private async Task HandleClientAsync(ClientConnection connection)
    {
        Console.Error.WriteLine("Local client connected");
        try
        {
            await connection.RunAsync();
        }
        catch (Exception error)
        {
            if (error is not OperationCanceledException)
                Console.Error.WriteLine($"Local client ended: {error.Message}");
        }
        finally
        {
            connection.Dispose();
            lock (stateLock)
            {
                if (ReferenceEquals(client, connection)) client = null;
            }
            Console.Error.WriteLine("Local client disconnected; scope connection retained if healthy");
        }
    }

    /// <summary>Reads complete video packets, preserving headers and draining payloads even with no local client.</summary>
    private async Task ReadVideoAsync(ScopeConnection connection)
    {
        var stream = connection.Socket.GetStream();
        var header = new byte[HeaderSize];
        var discard = new byte[65536];
        while (true)
        {
            await stream.ReadExactlyAsync(header, connection.Token);
            var length = PayloadLength(header);
            ClientConnection? target;
            lock (stateLock)
            {
                target = client?.Scope == connection ? client : null;
            }
            if (target is null)
            {
                for (var remaining = length; remaining > 0;)
                {
                    var count = Math.Min(remaining, discard.Length);
                    await stream.ReadExactlyAsync(discard.AsMemory(0, count), connection.Token);
                    remaining -= count;
                }
                continue;
            }

            var packet = new byte[HeaderSize + length];
            header.CopyTo(packet, 0);
            await stream.ReadExactlyAsync(packet.AsMemory(HeaderSize), connection.Token);
            target.OfferVideo(packet);
        }
    }

    /// <summary>Validates only the known magic and bounded length; the unknown header bytes are preserved.</summary>
    private static int PayloadLength(byte[] header)
    {
        if (header[2] != 0x55 || header[3] != 0xaa)
            throw new InvalidDataException("Invalid video packet magic; refusing to guess a new boundary");
        var length = BinaryPrimitives.ReadUInt32LittleEndian(header.AsSpan(4));
        if (length > MaxPayloadSize)
            throw new InvalidDataException($"Video payload exceeds {MaxPayloadSize} bytes");
        return (int)length;
    }

    /// <summary>Owns an upstream session and invalidates it if a control write cannot finish safely.</summary>
    private sealed class ScopeConnection : IDisposable
    {
        private readonly CancellationTokenSource lifetime;
        private int closed;
        public TcpClient Socket { get; } = new() { NoDelay = true };
        public CancellationToken Token { get; }

        /// <summary>Links upstream lifetime to relay shutdown, independently of downstream clients.</summary>
        public ScopeConnection(CancellationToken cancellationToken)
        {
            lifetime = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken);
            Token = lifetime.Token;
        }

        /// <summary>Finishes one whole input event even if the local client departs; never retries it on a new scope connection.</summary>
        public async Task SendEventAsync(byte[] packet)
        {
            using var writing = CancellationTokenSource.CreateLinkedTokenSource(Token);
            writing.CancelAfter(WriteTimeout);
            try
            {
                await Socket.GetStream().WriteAsync(packet, writing.Token);
            }
            catch
            {
                // A failed write may have sent only part of the 12 bytes; a fresh session must not inherit that prefix.
                Close();
                throw;
            }
        }

        /// <summary>Interrupts upstream I/O exactly once, including concurrent read and write failures.</summary>
        public void Close()
        {
            if (Interlocked.Exchange(ref closed, 1) != 0) return;
            lifetime.Cancel();
            Socket.Dispose();
        }

        /// <summary>Releases the connection and its cancellation registrations after the session ends.</summary>
        public void Dispose()
        {
            Close();
            lifetime.Dispose();
        }
    }

    /// <summary>Owns the sole local client, with a bounded video queue so it cannot stall the upstream reader.</summary>
    private sealed class ClientConnection : IDisposable
    {
        private const int MaxQueuedBytes = 32 * 1024 * 1024;
        private readonly TcpClient socket;
        private readonly CancellationTokenSource lifetime;
        private readonly CancellationToken token;
        private readonly Channel<byte[]> video = Channel.CreateBounded<byte[]>(new BoundedChannelOptions(32)
        {
            SingleReader = true,
            SingleWriter = true,
            FullMode = BoundedChannelFullMode.Wait,
        });
        private int closed;
        private int queuedBytes;
        public ScopeConnection Scope { get; }

        /// <summary>Binds input to this upstream generation so disconnected clients cannot control a new scope session.</summary>
        public ClientConnection(TcpClient socket, ScopeConnection scope, CancellationToken cancellationToken)
        {
            this.socket = socket;
            Scope = scope;
            lifetime = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken);
            token = lifetime.Token;
        }

        /// <summary>Queues a whole video packet or disconnects a slow client instead of dropping H.264 dependencies.</summary>
        public void OfferVideo(byte[] packet)
        {
            if (token.IsCancellationRequested) return;
            if (Interlocked.Add(ref queuedBytes, packet.Length) > MaxQueuedBytes)
            {
                Interlocked.Add(ref queuedBytes, -packet.Length);
                Console.Error.WriteLine("Local client video backlog exceeds 32 MiB; disconnecting it");
                Close();
                return;
            }
            if (!video.Writer.TryWrite(packet))
            {
                Interlocked.Add(ref queuedBytes, -packet.Length);
                Console.Error.WriteLine("Local client cannot keep up with video; disconnecting it");
                Close();
            }
        }

        /// <summary>Runs both directions and cancels the sibling pump when either direction ends.</summary>
        public async Task RunAsync()
        {
            var input = ForwardInputAsync();
            var output = ForwardVideoAsync();
            try
            {
                await Task.WhenAny(input, output);
            }
            finally
            {
                Close();
                await Task.WhenAll(input, output);
            }
        }

        /// <summary>Assembles exactly 12 bytes per event and discards any incomplete tail on disconnect.</summary>
        private async Task ForwardInputAsync()
        {
            var stream = socket.GetStream();
            var packet = new byte[EventSize];
            while (true)
            {
                await stream.ReadExactlyAsync(packet, token);
                await Scope.SendEventAsync(packet);
            }
        }

        /// <summary>Writes complete original video packets with bounded time spent on a slow client.</summary>
        private async Task ForwardVideoAsync()
        {
            var stream = socket.GetStream();
            await foreach (var packet in video.Reader.ReadAllAsync(token))
            {
                try
                {
                    using var writing = CancellationTokenSource.CreateLinkedTokenSource(token);
                    writing.CancelAfter(WriteTimeout);
                    await stream.WriteAsync(packet, writing.Token);
                }
                finally
                {
                    Interlocked.Add(ref queuedBytes, -packet.Length);
                }
            }
        }

        /// <summary>Interrupts local I/O without closing the scope session.</summary>
        public void Close()
        {
            if (Interlocked.Exchange(ref closed, 1) != 0) return;
            lifetime.Cancel();
            video.Writer.TryComplete();
            socket.Dispose();
        }

        /// <summary>Releases local connection resources after both pumps have finished.</summary>
        public void Dispose()
        {
            Close();
            while (video.Reader.TryRead(out var packet))
            {
                Interlocked.Add(ref queuedBytes, -packet.Length);
            }
            lifetime.Dispose();
        }
    }
}
