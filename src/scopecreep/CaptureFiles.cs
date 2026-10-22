using System.Security.Cryptography;
using System.Text;

/// <summary>A downloaded scope file retained temporarily until an explicit save.</summary>
internal sealed record DownloadedFile(string Id, string Name, long Size, string Sha256, string LocalPath,
    string SourceReference, DateTimeOffset DownloadedAtUtc);

/// <summary>Bounds downloads on disk and provides transport-independent chunk access and explicit persistence.</summary>
internal sealed class CaptureFiles(ScopeConfig config, MtpDevice mtp) : IDisposable
{
    private readonly string temporary = Path.Combine(Path.GetTempPath(), "scopecreep", Guid.NewGuid().ToString("N"));
    private readonly SemaphoreSlim gate = new(1);
    private readonly Dictionary<string, DownloadedFile> files = new();
    private long total;
    private bool disposed;

    /// <summary>Streams one MTP object to an unpublished partial file, then hashes and publishes it atomically.</summary>
    public async Task<DownloadedFile> FetchAsync(string reference, CancellationToken token)
    {
        await gate.WaitAsync(token);
        string? partial = null;
        try
        {
            ObjectDisposedException.ThrowIf(disposed, this);
            var limit = Math.Min(config.MaxFileBytes, config.MaxTemporaryBytes - total);
            if (limit <= 0) throw new IOException("Temporary capture quota exhausted; save selected files and restart to release temporary storage");
            Directory.CreateDirectory(temporary);
            if (OperatingSystem.IsLinux()) File.SetUnixFileMode(temporary, UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute);
            var id = Guid.NewGuid().ToString("N");
            partial = Path.Combine(temporary, id + ".part");
            MtpObject info;
            await using (var output = new FileStream(partial, FileMode.CreateNew, FileAccess.Write, FileShare.None, 65536, FileOptions.SequentialScan))
            {
                info = await mtp.DownloadAsync(reference, output, limit, token);
                await output.FlushAsync(token);
                if (output.Length != info.Size) throw new IOException("Downloaded file length mismatch");
            }
            string hash;
            await using (var input = File.OpenRead(partial)) hash = Convert.ToHexString(await SHA256.HashDataAsync(input, token)).ToLowerInvariant();
            var path = Path.Combine(temporary, id + ".capture");
            File.Move(partial, path);
            partial = null;
            var result = new DownloadedFile(id, info.Name, info.Size, hash, path, reference, DateTimeOffset.UtcNow);
            files.Add(id, result);
            total += info.Size;
            return result;
        }
        finally
        {
            if (partial is not null) File.Delete(partial);
            gate.Release();
        }
    }

    /// <summary>Reads a bounded byte range; text requires valid UTF-8 boundaries and base64 preserves arbitrary bytes.</summary>
    public async Task<object> ReadAsync(string id, long offset, int maxBytes, string format, CancellationToken token)
    {
        if (offset < 0 || maxBytes is < 1 or > 1048576 || format is not ("text" or "base64"))
            throw new ArgumentException("offset >= 0, maxBytes 1..1048576, format text or base64");
        await gate.WaitAsync(token);
        try
        {
            var file = GetFile(id);
            if (offset > file.Size) throw new ArgumentException("offset exceeds file size");
            var data = new byte[(int)Math.Min(maxBytes, file.Size - offset)];
            await using var input = File.OpenRead(file.LocalPath);
            input.Position = offset;
            await input.ReadExactlyAsync(data, token);
            string? text = null;
            if (format == "text")
            {
                try { text = new UTF8Encoding(false, true).GetString(data); }
                catch (DecoderFallbackException) { throw new ArgumentException("Chunk is not valid complete UTF-8; use base64 or adjust byte boundaries"); }
            }
            return new { downloadId = id, offset, byteCount = data.Length, nextOffset = offset + data.Length,
                eof = offset + data.Length == file.Size, text, base64 = format == "base64" ? Convert.ToBase64String(data) : null };
        }
        finally { gate.Release(); }
    }

    /// <summary>Saves a selected download under a plain filename without overwriting existing files.</summary>
    public async Task<object> SaveAsync(string id, string name, CancellationToken token)
    {
        ValidateName(name);
        await gate.WaitAsync(token);
        string? destination = null;
        var created = false;
        try
        {
            var file = GetFile(id);
            Directory.CreateDirectory(config.CaptureDirectory);
            destination = Path.Combine(config.CaptureDirectory, name);
            await using (var output = new FileStream(destination, FileMode.CreateNew, FileAccess.Write, FileShare.None))
            {
                created = true;
                await using var input = File.OpenRead(file.LocalPath);
                await input.CopyToAsync(output, token);
                await output.FlushAsync(token);
            }
            created = false;
            return new { downloadId = id, path = destination, file.Size, file.Sha256, temporaryCopyRetained = true };
        }
        finally
        {
            if (created && destination is not null) File.Delete(destination);
            gate.Release();
        }
    }

    /// <summary>Rejects path traversal and hidden directory operations in save requests.</summary>
    internal static void ValidateName(string name)
    {
        if (string.IsNullOrWhiteSpace(name) || name is "." or ".." || name.Length > 255 ||
            Path.GetFileName(name) != name || name.Contains('\\') || name.Any(char.IsControl))
            throw new ArgumentException("name must be a plain filename, without directories or control characters");
    }

    /// <summary>Looks up only downloads owned by this live MCP process.</summary>
    private DownloadedFile GetFile(string id)
    {
        ObjectDisposedException.ThrowIf(disposed, this);
        return files.TryGetValue(id, out var file) ? file : throw new ArgumentException("Unknown download ID; temporary files last for this MCP process");
    }

    /// <summary>Removes only this process's private temporary directory, preserving all explicitly saved files.</summary>
    public void Dispose()
    {
        gate.Wait();
        try
        {
            if (disposed) return;
            disposed = true;
            if (Directory.Exists(temporary)) Directory.Delete(temporary, recursive: true);
        }
        finally { gate.Release(); }
    }
}
