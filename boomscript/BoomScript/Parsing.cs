namespace BoomScript;

public class Parser
{
    private readonly SyntaxTree _tree;
    private readonly SyntaxToken[] _tokens;
    private int _position;

    public Diagnostics Diagnostics { get; } = new();
    public SyntaxTree Tree => _tree;

    public Parser(SyntaxTree tree)
    {
        _tree = tree;
        _tokens = LexTokens();
    }

    public CompilationUnit ParseCompilationUnit()
    {
        var statements = ParseStatements();
        var endOfFileToken = MatchToken(SyntaxKind.EndOfFileToken);
        return new CompilationUnit(statements, endOfFileToken, _tree, new TextSpan(0, endOfFileToken.Span.End));
    }

    private Statement[] ParseStatements()
    {
        var statements = new List<Statement>();

        while (Current.Kind != SyntaxKind.EndOfFileToken)
            statements.Add(ParseStatement());
        
        return statements.ToArray();
    }

    private Statement ParseStatement()
    {
        throw new NotImplementedException();
    }

    private SyntaxToken Current => Peek(0);
    private SyntaxToken Lookahead => Peek(1);
    
    private SyntaxToken Peek(int offset)
    {
        if (_position + offset >= _tokens.Length)
            return _tokens[+_tokens.Length - 1];
        
        return _tokens[_position + offset];
    }

    private SyntaxToken MatchToken(SyntaxKind expected)
    {
        if (Current.Kind == expected)
            return NextToken();
        
        Diagnostics.ReportUnexpectedToken(Current.Location, Current.Kind, expected);
        return new SyntaxToken(_tree, expected, Current.Span);
    }

    private SyntaxToken NextToken()
    {
        _position++;
        return _tokens[_position - 1];
    }

    private SyntaxToken[] LexTokens()
    {
        var tokens = new List<SyntaxToken>();
        var lexer = new Lexer(_tree);

        while (true)
        {
            switch (lexer.Lex())
            {
                case { Kind: SyntaxKind.BadToken }:
                    // Consider combining bad tokens into fake token in tree
                    continue;
                case { Kind: SyntaxKind.EndOfFileToken }:
                    return tokens.ToArray();
                case var token:
                    tokens.Add(token);
                    break;
            }
        }
    }
}