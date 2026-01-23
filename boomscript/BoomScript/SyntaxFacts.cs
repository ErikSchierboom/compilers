namespace BoomScript;

public static class SyntaxFacts
{
    extension(SyntaxKind kind)
    {
        public int GetUnaryOperatorPrecedence() =>
            kind switch
            {
                SyntaxKind.PlusToken or SyntaxKind.MinusToken => 6,
                _ => 0
            };

        public int GetBinaryOperatorPrecedence() =>
            kind switch
            {
                SyntaxKind.StarToken or SyntaxKind.SlashToken => 5,
                SyntaxKind.PlusToken or SyntaxKind.MinusToken => 4,
                _ => 0
            };
    }
}