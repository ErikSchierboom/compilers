namespace BoomScript;

public class Lexer(SyntaxTree tree)
{
    private int _position;
    private int _start;
    private SyntaxKind _kind;
    
    public SyntaxTree Tree => tree;
    public Diagnostics Diagnostics { get; } = new();
    
    public SyntaxToken Lex()
    {
        while (char.IsWhiteSpace(Current))
            _position++;
        
        _start = _position;
        
        switch (Current)
        {
            case '+':
                _kind = SyntaxKind.PlusToken;
                _position++;
                break;
            case '-':
                _kind = SyntaxKind.MinusToken;
                _position++;
                break;
            case '*':
                _kind = SyntaxKind.StarToken;
                _position++;
                break;
            case '/':
                _kind = SyntaxKind.SlashToken;
                _position++;
                break;
            case '(':
                _kind = SyntaxKind.OpenParenthesisToken;
                _position++;
                break;
            case ')':
                _kind = SyntaxKind.CloseParenthesisToken;
                _position++;
                break;
            case '0' or '1' or '2' or '3' or '4' or '5' or '6' or '7' or '8' or '9':
                _kind = SyntaxKind.NumberToken;
                while (char.IsDigit(Current))
                    _position++;
                break;
            case '\0':
                _kind = SyntaxKind.EndOfFileToken;
                break;
            default:
                _kind = SyntaxKind.BadToken;
                _position++;
                Diagnostics.ReportBadCharacter(new TextLocation(tree.Text, new TextSpan(_start, _position)), Current);
                break;
        }

        return new SyntaxToken(tree, _kind, new TextSpan(_start, _position - _start));
    }

    private char Current => _position < tree.Text.Length ? tree.Text[_position] : '\0';
}