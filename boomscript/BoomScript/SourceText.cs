namespace BoomScript;

public record TextSpan(int Start, int Length);

public record SourceText(string Text)
{
     public int Length => Text.Length;
     public char this[int index] => Text[index];
     public string this[TextSpan span] => Text[span.Start..span.Length];
}