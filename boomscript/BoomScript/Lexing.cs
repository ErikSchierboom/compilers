namespace BoomScript;

public class Lexer(SyntaxTree tree)
{
    public Diagnostics Diagnostics { get; } = new();
    private SourceText SourceText => tree.SourceText;

    private SyntaxToken[] Lex()
    {
        var tokens = new List<SyntaxToken>();
        var position = 0;

        while (position < SourceText.Length)
        {
            switch (SourceText[position])
            {
                case ' ' or '\t' or '\r':
                    position++;
                    break;
                case '\n':
                    tokens.Add(new SyntaxToken(tree, SyntaxKind.NewlineToken, new TextSpan(position, 1)));
                    position++;
                    break;
                case '+':
                    tokens.Add(new SyntaxToken(tree, SyntaxKind.PlusToken, new TextSpan(position, 1)));
                    position++;
                    break;
                case '-':
                    tokens.Add(new SyntaxToken(tree, SyntaxKind.MinusToken, new TextSpan(position, 1)));
                    position++;
                    break;
                case '*':
                    tokens.Add(new SyntaxToken(tree, SyntaxKind.StarToken, new TextSpan(position, 1)));
                    position++;
                    break;
                case '/':
                    tokens.Add(new SyntaxToken(tree, SyntaxKind.SlashToken, new TextSpan(position, 1)));
                    position++;
                    break;
                case '=':
                    tokens.Add(new SyntaxToken(tree, SyntaxKind.EqualsToken, new TextSpan(position, 1)));
                    position++;
                    break;
                case ',':
                    tokens.Add(new SyntaxToken(tree, SyntaxKind.CommaToken, new TextSpan(position, 1)));
                    position++;
                    break;
                case '(':
                    tokens.Add(new SyntaxToken(tree, SyntaxKind.OpenParenthesisToken, new TextSpan(position, 1)));
                    position++;
                    break;
                case ')':
                    tokens.Add(new SyntaxToken(tree, SyntaxKind.CloseParenthesisToken, new TextSpan(position, 1)));
                    position++;
                    break;
                case var c when char.IsDigit(c):
                    var digitStartPosition = position;

                    while (position < SourceText.Length && char.IsDigit(SourceText[position]))
                        position++;
                    
                    tokens.Add(new SyntaxToken(tree, SyntaxKind.NumberToken, new TextSpan(digitStartPosition, position - digitStartPosition)));
                    break;
                case var c when char.IsAsciiLetter(c):
                    var identifierStartPosition = position;

                    while (position < SourceText.Length && char.IsAsciiLetterOrDigit(SourceText[position]))
                        position++;
                    
                    tokens.Add(new SyntaxToken(tree, SyntaxKind.IdentifierToken, new TextSpan(identifierStartPosition, position - identifierStartPosition)));
                    break;
                default:
                    Diagnostics.ReportBadCharacter(new TextLocation(SourceText, new TextSpan(position, 1)), SourceText[position]);
                    position++;
                    break;
            }
        }
        
        tokens.Add(new SyntaxToken(tree, SyntaxKind.EndOfFileToken, new TextSpan(SourceText.Length, 0)));
        
        return tokens.ToArray();
    }

    public static SyntaxToken[] Lex(SyntaxTree syntaxTree) => new Lexer(syntaxTree).Lex();
}