namespace BoomScript.Tests;

public sealed class SourceTextTests
{
    [Fact]
    public void GetLocationForSpan()
    {
        var sourceText = new SourceText("a\nbb\n\nccc\r\ndddd");

        var locationAtStartOfLine = sourceText.GetLocation(new TextSpan(0, 1));
        Assert.Equal(new TextPosition(1, 1), locationAtStartOfLine.Start);
        Assert.Equal(new TextPosition(1, 2), locationAtStartOfLine.End);
        
        var locationAtEndOfLine = sourceText.GetLocation(new TextSpan(3, 1));
        Assert.Equal(new TextPosition(2, 2), locationAtEndOfLine.Start);
        Assert.Equal(new TextPosition(2, 3), locationAtEndOfLine.End);
        
        var locationForEntireLine = sourceText.GetLocation(new TextSpan(6, 3));
        Assert.Equal(new TextPosition(4, 1), locationForEntireLine.Start);
        Assert.Equal(new TextPosition(4, 4), locationForEntireLine.End);
        
        var locationForPartialLine = sourceText.GetLocation(new TextSpan(12, 2));
        Assert.Equal(new TextPosition(5, 2), locationForPartialLine.Start);
        Assert.Equal(new TextPosition(5, 4), locationForPartialLine.End);
    }
}
