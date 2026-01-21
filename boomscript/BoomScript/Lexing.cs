namespace BoomScript;

public class Lexer(SyntaxTree tree)
{
    private readonly SourceText _text = tree.Text;
    private readonly List<SyntaxTrivia> _trivia = new();
    
    private int _position;
    private int _start;
    private SyntaxKind _kind;
    
    public SyntaxTree Tree => tree;
    public Diagnostics Diagnostics { get; } = new();
    
    public SyntaxToken Lex()
    {
        var leadingTrivia = ReadTrivia(leading: true);

        var tokenStart = _start;
        ReadToken();
        var tokenLength = _position - tokenStart;
        
        var trailingTrivia = ReadTrivia(leading: false);
            
        return new SyntaxToken(tree, _kind, new TextSpan(tokenStart, tokenLength), leadingTrivia, trailingTrivia);
    }

    private SyntaxTrivia[] ReadTrivia(bool leading)
    {
        _trivia.Clear();
        
        var done = false;

        while (!done)
        {
            _start = _position;
            
            switch (Current)
            {
                case '\0':
                    done = true;
                    break;
                case '\r' or '\n':
                    if (!leading)
                        done = true;

                    _kind = SyntaxKind.LineBreakTrivia;

                    if (Current == '\r' && Lookahead == '\n')
                        _position += 2;
                    else
                        _position++;
                    
                    break;
                case ' ' or '\t':
                case var c when char.IsWhiteSpace(c):
                    while (Current is not ('\0' or '\r' or '\n') && char.IsWhiteSpace(Current))
                        _position++;
                        
                    _kind = SyntaxKind.WhitespaceTrivia;
                    break;
                default:
                    done = true;
                    break;
            }
            
            if (_position == _start)
                continue;
            
            _trivia.Add(new SyntaxTrivia(tree, _kind, new TextSpan(_start, _position - _start)));
        }

        return _trivia.ToArray();
    }

    private void ReadToken()
    {   
        _start = _position;
        
        switch (Current)
        {
            case '\0':
                _kind = SyntaxKind.EndOfFileToken;
                break;
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
                while (char.IsDigit(Current))
                    _position++;
                break;
            default:
                _kind = SyntaxKind.BadToken;
                _position++;
                break;
        }
    }

    private char Current => Peek(0);
    private char Lookahead => Peek(1);
    
    private char Peek(int offset)
    {
        if (_position + offset >= _text.Length)
            return '\0';
        
        return _text[_position + offset];
    }
}