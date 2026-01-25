namespace BoomScript;

public enum Precedence
{
    None,
    Assignment,
    Conditional,
    Sum,
    Product,
    Exponent,
    Prefix,
    Postfix,
    Call,
}

public record PrefixParselet(Func<Expression> Function, Precedence Precedence);
public record InfixParselet(Func<Expression, Expression> Function, Precedence Precedence);

public class Parser
{
    private readonly SyntaxTree _tree;
    private readonly SyntaxToken[] _tokens;
    private readonly Dictionary<SyntaxKind, PrefixParselet> _prefixParser = new();
    private readonly Dictionary<SyntaxKind, InfixParselet> _infixParser = new();
    
    private int _position;

    public Diagnostics Diagnostics { get; } = new();

    public Parser(SyntaxTree tree)
    {
        _tree = tree;
        _tokens = Lexer.Lex(tree);

        _prefixParser[SyntaxKind.OpenParenthesisToken] = new(ParseParenthesizeExpression, Precedence.None);
        _prefixParser[SyntaxKind.NumberToken] = new(ParseNumberLiteral, Precedence.None);
        _prefixParser[SyntaxKind.IdentifierToken] = new(ParseNameExpression, Precedence.None);
        _prefixParser[SyntaxKind.MinusToken] = new(ParseUnaryExpression, Precedence.Prefix);
        _prefixParser[SyntaxKind.PlusToken] = new(ParseUnaryExpression, Precedence.Prefix);
        
        _infixParser[SyntaxKind.PlusToken] = new(ParseBinaryExpression, Precedence.Sum);
        _infixParser[SyntaxKind.MinusToken] = new(ParseBinaryExpression, Precedence.Sum);
        _infixParser[SyntaxKind.StarToken] = new(ParseBinaryExpression, Precedence.Product);
        _infixParser[SyntaxKind.SlashToken] = new(ParseBinaryExpression, Precedence.Product);
        _infixParser[SyntaxKind.OpenParenthesisToken] = new(ParseCallExpression, Precedence.Call);
    }

    public CompilationUnit ParseCompilationUnit()
    {
        var statements = ParseStatements();
        var endOfFileToken = MatchToken(SyntaxKind.EndOfFileToken);
        return new CompilationUnit(statements, _tree, new TextSpan(0, endOfFileToken.Span.End));
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
        if (Current.Kind == SyntaxKind.IdentifierToken && Lookahead.Kind == SyntaxKind.EqualsToken)
            return ParseVariableDeclarationStatement();
        
        return ParseExpressionStatement();
    }

    private Statement ParseExpressionStatement()
    {   
        var expression = ParseExpression(Precedence.None);
        MatchToken(SyntaxKind.NewlineToken);
        return new ExpressionStatement(expression, _tree, expression.Span);
    }
    
    private Statement ParseVariableDeclarationStatement()
    {
        var identifier = MatchToken(SyntaxKind.IdentifierToken);
        var equalsToken = MatchToken(SyntaxKind.EqualsToken);
        var initializer = ParseExpression(Precedence.None);
        MatchToken(SyntaxKind.NewlineToken);
        return new VariableDeclarationStatement(identifier, initializer, _tree, initializer.Span);
    }

    private Expression ParseExpression(Precedence precedence)
    {
        var prefix = _prefixParser[Current.Kind];
        var left = prefix.Function();

        while (Current.Kind is not (SyntaxKind.NewlineToken or SyntaxKind.EndOfFileToken))
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
        var expression = ParseExpression(Precedence.None);
        var closeToken = MatchToken(SyntaxKind.CloseParenthesisToken);
        return new ParenthesizedExpression(expression, _tree, new TextSpan(openToken.Span.Start, closeToken.Span.End - openToken.Span.Start));
    }

    private Expression ParseNumberLiteral()
    {
        var token = MatchToken(SyntaxKind.NumberToken);
        var value = int.Parse(token.Text);
        return new LiteralExpression(token, value, _tree, token.Span);
    }
    
    private Expression ParseNameExpression()
    {
        var token = MatchToken(SyntaxKind.IdentifierToken);
        return new NameExpression(token, _tree, token.Span);
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
    
    private Expression ParseCallExpression(Expression left)
    {
        var openParenthesisToken = MatchToken(SyntaxKind.OpenParenthesisToken);
        var args = new List<Expression>();

        if (Current.Kind != SyntaxKind.CloseParenthesisToken)
        {
            do
            {
                args.Add(ParseExpression(Precedence.None));
            } while (Current.Kind == SyntaxKind.CommaToken);
        }

        var closeParenthesisToken = MatchToken(SyntaxKind.CloseParenthesisToken);
        return new CallExpression(left, args.ToArray(), _tree, left.Span.Combine(closeParenthesisToken.Span));
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
}