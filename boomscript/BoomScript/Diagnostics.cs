using System.Collections;

namespace BoomScript;

public sealed record Diagnostic(string Message, DiagnosticSeverity Severity, TextLocation Location);

public sealed class Diagnostics : IEnumerable<Diagnostic>
{
    private List<Diagnostic> _diagnostics = new();
    
    public IEnumerator<Diagnostic> GetEnumerator() => _diagnostics.GetEnumerator();
    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
}

public enum DiagnosticSeverity
{
    Warning,
    Error
}
