namespace BoomScript;
public enum Precedence {
    NONE        = 0,
    ASSIGNMENT  = 1,
    CONDITIONAL = 2,
    SUM         = 3,
    PRODUCT     = 4,
    EXPONENT    = 5,
    PREFIX      = 6,
    POSTFIX     = 7,
    CALL        = 8,
}

public record PrefixParselet(Func<Expression> Function, Precedence Precedence);
public record InfixParselet(Func<Expression, Expression> Function, Precedence Precedence);

public class Parser
{
    private readonly SyntaxTree _tree;
    private readonly SyntaxToken[] _tokens;
    private int _position;
    private Dictionary<SyntaxKind, PrefixParselet> _prefixParser = new();
    private Dictionary<SyntaxKind, InfixParselet> _infixParser = new();

    public Diagnostics Diagnostics { get; } = new();
    public SyntaxTree Tree => _tree;

    public Parser(SyntaxTree tree)
    {
        _tree = tree;
        _tokens = LexTokens();

        _prefixParser[SyntaxKind.OpenParenthesisToken] = new(ParseParenthesizeExpression, Precedence.NONE);
        _prefixParser[SyntaxKind.NumberToken] = new(ParseNumberLiteral, Precedence.NONE);
        _prefixParser[SyntaxKind.MinusToken] = new(ParseUnaryExpression, Precedence.PREFIX);
        _prefixParser[SyntaxKind.PlusToken] = new(ParseUnaryExpression, Precedence.PREFIX);
        
        _infixParser[SyntaxKind.PlusToken] = new(ParseBinaryExpression, Precedence.SUM);
        _infixParser[SyntaxKind.MinusToken] = new(ParseBinaryExpression, Precedence.SUM);
        _infixParser[SyntaxKind.StarToken] = new(ParseBinaryExpression, Precedence.PRODUCT);
        _infixParser[SyntaxKind.SlashToken] = new(ParseBinaryExpression, Precedence.PRODUCT);
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
        var expression = ParseExpression(Precedence.NONE);
        return new ExpressionStatement(expression, _tree, expression.Span);
    }

    private Expression ParseExpression(Precedence precedence)
    {
        var prefix = _prefixParser[Current.Kind];
        var left = prefix.Function();

        while (Current.Kind != SyntaxKind.EndOfFileToken)
        {
            var infix = _infixParser[Current.Kind];
            if (infix.Precedence < precedence)
                return left;
            
            left = infix.Function(left);
        }

        return left;
    }

    private Expression ParseParenthesizeExpression()
    {
        var openToken = MatchToken(SyntaxKind.OpenParenthesisToken);
        var expression = ParseExpression(Precedence.NONE);
        var closeToken = MatchToken(SyntaxKind.CloseParenthesisToken);
        return new ParenthesizedExpression(openToken, expression, closeToken, _tree, new TextSpan(openToken.Span.Start, closeToken.Span.End - openToken.Span.Start));
    }

    private Expression ParseNumberLiteral()
    {
        var token = MatchToken(SyntaxKind.NumberToken);
        var value = int.Parse(token.Text);
        return new LiteralExpression(token, value, _tree, token.Span);
    }

    private Expression ParseUnaryExpression()
    {
        switch (Current.Kind)
        {
            case SyntaxKind.PlusToken or SyntaxKind.MinusToken:
                var token = MatchToken(Current.Kind);
                var precedence = _prefixParser[token.Kind].Precedence;
                var expr = ParseExpression(precedence);
                return new UnaryExpression(token, expr, _tree, token.Span.Combine(expr.Span));
            default:
                throw new ArgumentOutOfRangeException();
        }
    }

    private Expression ParseBinaryExpression(Expression left)
    {
        var operatorToken = NextToken();
        var right = ParseExpression(_infixParser[operatorToken.Kind].Precedence);
        return new BinaryExpression(left, operatorToken, right, _tree, left.Span.Combine(right.Span));
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