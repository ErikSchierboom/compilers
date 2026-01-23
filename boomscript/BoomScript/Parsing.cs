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
        return ParseExpressionStatement();
    }

    private Statement ParseExpressionStatement()
    {
        var expression = ParseExpression();
        return new ExpressionStatement(expression, _tree, expression.Span);
    }

    private Expression ParseExpression()
    {
        return ParseBinaryExpression();
    }

    private Expression ParsePrimaryExpression()
    {
        switch (Current.Kind)
        {
            case SyntaxKind.NumberToken:
                return ParseNumberLiteral();
            case SyntaxKind.OpenParenthesisToken:
                return ParseParenthesizeExpression();
            default:
                throw new ArgumentOutOfRangeException();
        }
    }

    private Expression ParseParenthesizeExpression()
    {
        var openToken = MatchToken(SyntaxKind.OpenParenthesisToken);
        var expression = ParseExpression();
        var closeToken = MatchToken(SyntaxKind.CloseParenthesisToken);
        return new ParenthesizedExpression(openToken, expression, closeToken, _tree, new TextSpan(openToken.Span.Start, closeToken.Span.End - openToken.Span.Start));
    }

    private Expression ParseNumberLiteral()
    {
        var token = MatchToken(SyntaxKind.NumberToken);
        var value = int.Parse(token.Text);
        return new LiteralExpression(token, value, _tree, token.Span);
    }

    private Expression ParseBinaryExpression(int parentPrecedence = 0)
    {
        Expression left;
        var unaryOperatorPrecedence = Current.Kind.GetUnaryOperatorPrecedence();
        if (unaryOperatorPrecedence != 0 && unaryOperatorPrecedence >= parentPrecedence)
        {
            var operatorToken = NextToken();
            var operand = ParseBinaryExpression(unaryOperatorPrecedence);
            left = new UnaryExpression(operatorToken, operand, _tree, operatorToken.Span.Combine(operand.Span));
        }
        else
        {
            left = ParsePrimaryExpression();
        }

        while (true)
        {
            var precedence = Current.Kind.GetBinaryOperatorPrecedence();
            if (precedence == 0 || precedence <= parentPrecedence)
                break;

            var operatorToken = NextToken();
            var right = ParseBinaryExpression(precedence);
            left = new BinaryExpression(left, operatorToken, right, _tree, left.Span.Combine(right.Span));
        }

        return left;
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
            var token = lexer.Lex();
            if (token.Kind == SyntaxKind.BadToken)
                continue;
            
            tokens.Add(token);
            
            if (token.Kind == SyntaxKind.EndOfFileToken)
                return tokens.ToArray();
        }
    }
}