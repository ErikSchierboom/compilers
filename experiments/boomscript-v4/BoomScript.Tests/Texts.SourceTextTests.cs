namespace BoomScript.Tests;

public sealed class TextLocationTests
{
    [Fact]
    public void LocationHasCorrectLineAndColumn()
    {
        var sourceText = new SourceText("a\nbb\n\nccc\r\ndddd");

        var locationAtStartOfLine = new TextLocation(sourceText, new TextSpan(0, 1));
        Assert.Equal(new TextPosition(1, 1), locationAtStartOfLine.Start);
        Assert.Equal(new TextPosition(1, 2), locationAtStartOfLine.End);
        
        var locationAtEndOfLine = new TextLocation(sourceText, new TextSpan(3, 1));
        Assert.Equal(new TextPosition(2, 2), locationAtEndOfLine.Start);
        Assert.Equal(new TextPosition(2, 3), locationAtEndOfLine.End);
        
        var locationForEntireLine = new TextLocation(sourceText, new TextSpan(6, 3));
        Assert.Equal(new TextPosition(4, 1), locationForEntireLine.Start);
        Assert.Equal(new TextPosition(4, 4), locationForEntireLine.End);
        
        var locationForPartialLine = new TextLocation(sourceText, new TextSpan(12, 2));
        Assert.Equal(new TextPosition(5, 2), locationForPartialLine.Start);
        Assert.Equal(new TextPosition(5, 4), locationForPartialLine.End);
    }
}
