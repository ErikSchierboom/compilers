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
        // TODO: implement
        return [];
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
        return new SyntaxToken(_tree, expected, Current.Span, Array.Empty<SyntaxTrivia>(), Array.Empty<SyntaxTrivia>());
    }

    private SyntaxToken NextToken()
    {
        _position++;
        return _tokens[_position - 1];
    }

    private SyntaxToken[] LexTokens()
    {
        var tokens = new List<SyntaxToken>();
        var badTokens = new List<SyntaxToken>();
        var lexer = new Lexer(_tree);

        SyntaxToken token;

        do
        {
            token = lexer.Lex();
            if (token.Kind == SyntaxKind.BadToken)
            {
                badTokens.Add(token);
            }
            else
            {
                var leadingTrivia = token.LeadingTrivia.ToList();
                var index = 0;
                
                if (badTokens.Count > 0)
                {
                    foreach (var badToken in badTokens)
                    {
                        foreach (var badTokenLeadingTrivia in badToken.LeadingTrivia)
                            leadingTrivia.Insert(index++, badTokenLeadingTrivia);
                        
                        leadingTrivia.Insert(index++, new SyntaxTrivia(_tree, SyntaxKind.BadTextTrivia, badToken.Span));
                        
                        foreach (var badTokenTrailingTrivia in badToken.TrailingTrivia)
                            leadingTrivia.Insert(index++, badTokenTrailingTrivia);
                    }
                    
                    badTokens.Clear();
                    token = token with { LeadingTrivia = leadingTrivia.ToArray() };
                }

                tokens.Add(token);
            }
        } while (token.Kind != SyntaxKind.EndOfFileToken);
        
        return tokens.ToArray();
    }
}