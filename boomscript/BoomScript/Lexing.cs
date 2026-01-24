namespace BoomScript;

public class Lexer(SyntaxTree tree)
{
    public Diagnostics Diagnostics { get; } = new();
    private SourceText SourceText => tree.SourceText;

    private SyntaxToken[] Lex()
    {
        var tokens = new List<SyntaxToken>();

        for (var position = 0; position < SourceText.Length; position++)
        {
            switch (SourceText[position])
            {
                case ' ' or '\t' or '\n' or 'r':
                    continue;
                case '+':
                    tokens.Add(new SyntaxToken(tree, SyntaxKind.PlusToken, new TextSpan(position, 1)));
                    break;
                case '-':
                    tokens.Add(new SyntaxToken(tree, SyntaxKind.MinusToken, new TextSpan(position, 1)));
                    break;
                case '*':
                    tokens.Add(new SyntaxToken(tree, SyntaxKind.StarToken, new TextSpan(position, 1)));
                    break;
                case '/':
                    tokens.Add(new SyntaxToken(tree, SyntaxKind.SlashToken, new TextSpan(position, 1)));
                    break;
                case '(':
                    tokens.Add(new SyntaxToken(tree, SyntaxKind.OpenParenthesisToken, new TextSpan(position, 1)));
                    break;
                case ')':
                    tokens.Add(new SyntaxToken(tree, SyntaxKind.CloseParenthesisToken, new TextSpan(position, 1)));
                    break;
                case '0' or '1' or '2' or '3' or '4' or '5' or '6' or '7' or '8' or '9':
                    var start = position;

                    while (position < SourceText.Length && char.IsDigit(SourceText[position]))
                        position++;
                    
                    tokens.Add(new SyntaxToken(tree, SyntaxKind.NumberToken, new TextSpan(start, position - start)));
                    break;
                default:
                    Diagnostics.ReportBadCharacter(new TextLocation(SourceText, new TextSpan(position, 1)), SourceText[position]);
                    break;
            }
        }
        
        tokens.Add(new SyntaxToken(tree, SyntaxKind.EndOfFileToken, new TextSpan(SourceText.Length, 0)));
        
        return tokens.ToArray();
    }

    public static SyntaxToken[] Lex(SyntaxTree syntaxTree) => new Lexer(syntaxTree).Lex();
}