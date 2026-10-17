using System.Text;

/// <summary>Validates SCPI messages without guessing the meaning of firmware replies.</summary>
internal static class ScpiProtocol
{
    /// <summary>Encodes one command, preventing compound statements and query/command mismatches.</summary>
    public static byte[] Encode(string command, bool query)
    {
        if (string.IsNullOrWhiteSpace(command) || command.Length > 4096 ||
            command.Any(c => c is < ' ' or > '~' || c == ';'))
            throw new ArgumentException("SCPI must be one ASCII statement, at most 4096 characters, without line breaks or semicolons");
        var header = command.Trim().Split(' ', 2)[0];
        if (header.Contains('?') != query || (query && (!header.EndsWith('?') || header.Count(c => c == '?') != 1)))
            throw new ArgumentException(query ? "Expected a query header ending in ?" : "Use scpi_query for queries");
        return Encoding.ASCII.GetBytes(command.Trim() + "\n");
    }

    /// <summary>Recognizes the observed firmware error, including replies without a newline.</summary>
    public static bool IsError(byte[] reply) => Text(reply).StartsWith("ErrorEnum:", StringComparison.Ordinal);

    /// <summary>Removes only the ASCII transport terminator.</summary>
    public static string Text(byte[] reply) => Encoding.ASCII.GetString(reply).TrimEnd('\r', '\n');
}
