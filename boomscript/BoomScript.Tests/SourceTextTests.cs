namespace BoomScript.Tests;

public sealed class SourceTextTests
{
    [Fact]
    public void GetPositionInSourceTextBySpan()
    {
        var sourceText = new SourceText("a\nbb\n\nccc\r\ndddd");

        var singleCharAtStartOfLine = sourceText.GetLocation(new TextSpan(0, 1));
        Assert.Equal(new TextPosition(1, 1), singleCharAtStartOfLine.Start);
        Assert.Equal(new TextPosition(1, 2), singleCharAtStartOfLine.End);
        
        var lastCharAtEndOfLine = sourceText.GetLocation(new TextSpan(3, 1));
        Assert.Equal(new TextPosition(2, 2), lastCharAtEndOfLine.Start);
        Assert.Equal(new TextPosition(2, 3), lastCharAtEndOfLine.End);
        
        var allCharsOnLine = sourceText.GetLocation(new TextSpan(6, 3));
        Assert.Equal(new TextPosition(4, 1), allCharsOnLine.Start);
        Assert.Equal(new TextPosition(4, 4), allCharsOnLine.End);
        
        var someCharsOnLine = sourceText.GetLocation(new TextSpan(12, 2));
        Assert.Equal(new TextPosition(5, 2), someCharsOnLine.Start);
        Assert.Equal(new TextPosition(5, 4), someCharsOnLine.End);
    }
}
