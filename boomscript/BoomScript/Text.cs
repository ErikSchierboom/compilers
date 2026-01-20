namespace BoomScript;

// TODO: consider storing source (file, REPL, whatever) with sourcetext

public sealed record SourceText(string Text)
{   
    public TextLocation GetLocation(TextSpan span) => new(Lines.GetPosition(span.Start), Lines.GetPosition(span.End));
    
    private TextLines Lines => field ??= new TextLines(this);
}

public sealed class TextLines
{
    private int[] _lineStarts;
    
    public TextLines(SourceText text)
    {
        var lineStarts = new List<int> { 0 };

        for (var i = 0; i < text.Text.Length; i++)
        {
            if (text.Text[i] == '\n')
                lineStarts.Add(i + 1);
        }
        
        _lineStarts = lineStarts.ToArray();
    }
    
    public TextPosition GetPosition(int position)
    {
        var lineIndex = Array.FindLastIndex(_lineStarts, lineStart => lineStart <= position);
        var lineStart = _lineStarts[lineIndex];
        return new TextPosition(lineIndex + 1, position - lineStart + 1);
    }
}

public sealed record TextSpan(int Start, int Length)
{
    public int End => Start + Length;
}

public sealed record TextLocation(TextPosition Start, TextPosition End);
public sealed record TextPosition(int Line, int Column);

public sealed record Diagnostic(string Message, TextLocation Location);
