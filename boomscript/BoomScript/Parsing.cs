namespace BoomScript;

public class Parser
{
    private readonly SyntaxTree _tree;
    private readonly SyntaxToken[] _tokens;

    public Diagnostics Diagnostics { get; } = new();
    public SyntaxTree Tree => _tree;

    public Parser(SyntaxTree tree)
    {
        _tree = tree;
        _tokens = LexTokens();
    }

    public CompilationUnit ParseCompilationUnit()
    {
        throw new NotImplementedException();
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