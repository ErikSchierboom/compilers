using System.Collections;

namespace BoomScript;

public sealed record Diagnostic(string Message, DiagnosticSeverity Severity, TextLocation Location);

public sealed class Diagnostics : IEnumerable<Diagnostic>
{
    private readonly List<Diagnostic> _diagnostics = new();
    
    public IEnumerator<Diagnostic> GetEnumerator() => _diagnostics.GetEnumerator();
    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
    
    public void ReportUnexpectedToken(TextLocation location, SyntaxKind actualKind, SyntaxKind expectedKind) =>
        ReportError(location, $"Unexpected token <{actualKind}>, expected <{expectedKind}>.");

    public void ReportBadCharacter(TextLocation location, char character) =>
        ReportError(location, $"Bad character input: '{character}'.");

    private void ReportError(TextLocation location, string message) =>
        _diagnostics.Add(new Diagnostic(message, DiagnosticSeverity.Error, location));

    private void ReportWarning(TextLocation location, string message) =>
        _diagnostics.Add(new Diagnostic(message, DiagnosticSeverity.Warning, location));
}

public enum DiagnosticSeverity
{
    Warning,
    Error
}
