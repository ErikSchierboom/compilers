namespace BoomScript;

// TODO: consider storing source (file, REPL, whatever) with sourcetext

public sealed record SourceText(string Text)
{   
    public TextPosition GetPosition(int position) => Lines.GetPosition(position);
    
    private TextLines Lines => field ??= new(this);

    public int Length => Text.Length;
    
    public char this[int index] => Text[index];
    public string this[Range index] => Text[index];
}

public sealed class TextLines
{
    private readonly int[] _lines;
    
    public TextLines(SourceText text)
    {
        var lines = new List<int> { 0 };

        for (var i = 0; i < text.Text.Length; i++)
        {
            if (text.Text[i] == '\n')
                lines.Add(i + 1);
        }
        
        _lines = lines.ToArray();
    }
    
    public TextPosition GetPosition(int position)
    {
        var lineIndex = GetLineIndex(position);
        return new(lineIndex + 1, position - _lines[lineIndex] + 1);
    }

    private int GetLineIndex(int position)
    {
        var lower = 0;
        var upper = _lines.Length - 1;

        while (lower <= upper)
        {
            var index = lower + (upper - lower) / 2;
            var lineStart = _lines[index];

            if (lineStart == position)
                return index;
            
            if (position < lineStart)
                upper = index - 1;
            else
                lower = index + 1;
        }

        return lower - 1;
    }
}

public sealed record TextSpan(int Start, int Length)
{
    public int End => Start + Length;
    
    public TextSpan Combine(TextSpan other) => new(Start, other.End - Start);
}

public sealed record TextLocation(SourceText Text, TextSpan Span)
{
    public TextPosition Start => field ??= Text.GetPosition(Span.Start);
    public TextPosition End => field ??= Text.GetPosition(Span.End);
}

public sealed record TextPosition(int Line, int Column);
