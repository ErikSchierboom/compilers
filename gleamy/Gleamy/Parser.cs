namespace Gleamy;

internal enum Precedence
{
    None,         // =
    LogicalOr,    // or
    LogicalAnd,   // and
    BitwiseOr,    // |
    BitwiseXor,   // ^
    BitwiseAnd,   // &
    Equality,     // == !=
    Comparison,   // < > <= >=
    BitwiseShift, // << >>
    Addition,     // + -
    Product,      // * /
    Match,        // match
    Unary,        // ! - + ~
    Call,         // ()
    Primary
}

internal delegate Expression ParsePrefix();
internal delegate Expression ParseInfix(Expression left);

internal record ParseRule(ParsePrefix? Prefix, ParseInfix? Infix, Precedence Precedence);

internal class Parser
{
    private readonly Dictionary<TokenType, ParseRule> _rules;
    private readonly List<Token> _tokens;
    private int _position;

    private Parser(List<Token> tokens)
    {
        _tokens = tokens;
        _rules = new()
        {
            [TokenType.Eof] = new(null, null, Precedence.None),
            [TokenType.EqualEqual] = new(null, ParseBinaryExpression, Precedence.Equality),
            [TokenType.Bang] = new(ParseUnaryExpression, null, Precedence.Unary),
            [TokenType.BangEqual] = new(null, ParseBinaryExpression, Precedence.Equality),
            [TokenType.Greater] = new(null, ParseBinaryExpression, Precedence.Comparison),
            [TokenType.GreaterEqual] = new(null, ParseBinaryExpression, Precedence.Comparison),
            [TokenType.Less] = new(null, ParseBinaryExpression, Precedence.Comparison),
            [TokenType.LessEqual] = new(null, ParseBinaryExpression, Precedence.Comparison),
            [TokenType.Plus] = new(ParseUnaryExpression, ParseBinaryExpression, Precedence.Addition),
            [TokenType.Minus] = new(ParseUnaryExpression, ParseBinaryExpression, Precedence.Addition),
            [TokenType.Star] = new(null, ParseBinaryExpression, Precedence.Product),
            [TokenType.Slash] = new(null, ParseBinaryExpression, Precedence.Product),
            [TokenType.Caret] = new(null, ParseBinaryExpression, Precedence.BitwiseXor),
            [TokenType.Tilde] = new(ParseUnaryExpression, null, Precedence.Unary),
            [TokenType.GreaterGreater] = new(null, ParseBinaryExpression, Precedence.BitwiseShift),
            [TokenType.LessLess] = new(null, ParseBinaryExpression, Precedence.BitwiseShift),
            [TokenType.Ampersand] = new(null, ParseBinaryExpression, Precedence.BitwiseAnd),
            [TokenType.AmpersandAmpersand] = new(null, ParseLogicalAndExpression, Precedence.LogicalAnd),
            [TokenType.Pipe] = new(null, ParseBinaryExpression, Precedence.BitwiseOr),
            [TokenType.PipePipe] = new(null, ParseLogicalOrExpression, Precedence.LogicalOr),
            [TokenType.Number] = new(ParseNumber, null, Precedence.Primary),
            [TokenType.Identifier] = new(ParseName, null, Precedence.Primary),
            [TokenType.TrueKeyword] = new(ParseBoolean, null, Precedence.Primary),
            [TokenType.FalseKeyword] = new(ParseBoolean, null, Precedence.Primary),
            [TokenType.MatchKeyword] = new(ParseMatchExpression, null, Precedence.Match),
            [TokenType.OpenParen] = new(ParseParenthesized, ParseCall, Precedence.Call),
        };
    }

    public static SyntaxTree Parse(string source)
    {
        var tokens = Scanner.Scan(source);
        return new Parser(tokens).Parse();
    }

    private SyntaxTree Parse()
    {
        var statements = new List<Statement>();

        while (!IsEndOfFile)
            statements.Add(ParseStatement());

        return new SyntaxTree(statements);
    }

    private Statement ParseStatement()
    {
        if (Match(TokenType.LetKeyword))
            return ParseBindingDeclarationStatement();

        if (Match(TokenType.FnKeyword))
            return ParseFunctionDeclarationStatement();
        
        return ParseExpressionStatement();
    }

    private BindingDeclarationStatement ParseBindingDeclarationStatement()
    {
        Consume(TokenType.Identifier);
        var identifier = Previous;
        Consume(TokenType.Equal);
        var expression = ParseExpression();
        Match(TokenType.Semicolon);
        return new BindingDeclarationStatement(identifier, expression);
    }

    private FunctionDeclarationStatement ParseFunctionDeclarationStatement()
    {
        Consume(TokenType.Identifier);
        var identifier = Previous;

        Consume(TokenType.OpenParen);
        var parameters = new List<Parameter>();
        while (Current.Type != TokenType.CloseParen)
        {
            parameters.Add(ParseParameter());

            if (!Match(TokenType.Comma))
                break;
        }
        Consume(TokenType.CloseParen);

        Consume(TokenType.MinusGreater);
        var returnType = ParseType();
        
        var body = ParseBlockStatement();
        
        return new FunctionDeclarationStatement(identifier, [..parameters], returnType, body);
    }

    private BlockStatement ParseBlockStatement()
    {
        Consume(TokenType.OpenBracket);
        
        var statements = new List<Statement>();
        while (Current.Type != TokenType.CloseBracket)
            statements.Add(ParseStatement());
         
        Consume(TokenType.CloseBracket);
        
        return new BlockStatement([..statements]);
    }

    private IdentifierType ParseType()
    {
        if (Match(TokenType.IntKeyword) || Match(TokenType.BoolKeyword))
            return new IdentifierType(Previous);
        
        throw new InvalidOperationException($"Expected type but got {Current.Type}");
    }

    private Parameter ParseParameter()
    {
        Consume(TokenType.Identifier);
        var identifier = Previous;
        Consume(TokenType.Colon);
        var type = ParseType();
        return new Parameter(identifier, type);
    }

    private ExpressionStatement ParseExpressionStatement()
    {
        var expression = ParseExpression();
        Match(TokenType.Semicolon);
        return new ExpressionStatement(expression);
    }

    private Expression ParseExpression(Precedence precedence = Precedence.None)
    {
        var parsePrefixFn = CurrentParseRule.Prefix ?? throw new InvalidOperationException("Expected prefix");
        Consume();
        var left = parsePrefixFn();
        
        while (precedence < CurrentPrecedence)
        {
            var parseInfixFn = CurrentParseRule.Infix ?? throw new InvalidOperationException("Expected infix");
            Consume();
            left = parseInfixFn(left);
        }

        return left;
    }

    private Expression ParseLogicalOrExpression(Expression left)
    {   
        var operatorToken = Previous;
        var rule = _rules[operatorToken.Type];
        
        var right = ParseExpression(rule.Precedence + 1);
        return new LogicalOrExpression(left, right);
    }
    
    private Expression ParseLogicalAndExpression(Expression left)
    {
        var operatorToken = Previous;
        var rule = _rules[operatorToken.Type];
        
        var right = ParseExpression(rule.Precedence + 1);
        return new LogicalAndExpression(left, right);
    }
    
    private Expression ParseBinaryExpression(Expression left)
    {
        var operatorToken = Previous;
        var rule = _rules[operatorToken.Type];
        var right = ParseExpression(rule.Precedence);

        return new BinaryExpression(left, operatorToken, right);
    }

    private Expression ParseMatchExpression()
    {
        var input = ParseExpression(Precedence.Match + 1);
        Consume(TokenType.OpenBracket);
    
        var cases = new List<MatchCase>();
    
        while (Current.Type != TokenType.CloseBracket)
        {
            do
            {
                cases.Add(ParseMatchCase());
            } while (Match(TokenType.Comma));
        }
        
        Consume(TokenType.CloseBracket);
    
        return new MatchExpression(input, [..cases]);
    }

    private MatchCase ParseMatchCase()
    {
        var pattern = ParseMatchPattern();
        Consume(TokenType.EqualGreater);
        var returnValue = ParseExpression();
        return new MatchCase(pattern, returnValue);
    }

    private MatchPattern ParseMatchPattern()
    {
        if (Match(TokenType.Underscore))
            return new DiscardPattern();

        if (Match(TokenType.Identifier))
            return new BindingMatchPattern(Previous);

        if (Match(TokenType.Number) || Match(TokenType.TrueKeyword) || Match(TokenType.FalseKeyword))
            return new ConstantMatchPattern(Previous);

        if (Match(TokenType.Bang))
        {
            if (Match(TokenType.Number) || Match(TokenType.TrueKeyword) || Match(TokenType.FalseKeyword))
                return new NegationMatchPattern(Previous);
            
            throw new InvalidOperationException("Expected constant after '!'");
        }

        if (Match(TokenType.Greater) || Match(TokenType.GreaterEqual) ||
            Match(TokenType.Less) || Match(TokenType.LessEqual))
        {
            var operatorToken = Previous;
            var compareValue = ParseExpression(Precedence.Primary + 1);
            if (compareValue is not LiteralExpression literal)
                throw new InvalidOperationException($"Unexpected token {compareValue}");

            return new ComparisonMatchPattern(operatorToken, literal.Value);
        }

        throw new InvalidOperationException("Expected pattern");
    }

    private Expression ParseUnaryExpression() => new UnaryExpression(Previous, ParseExpression());

    private Expression ParseNumber() => new LiteralExpression(Previous);
    private Expression ParseBoolean() => new LiteralExpression(Previous);
    private Expression ParseName() => new NameExpression(Previous);
    private Expression ParseParenthesized()
    {
        var expr = ParseExpression();
        Consume(TokenType.CloseParen);
        return new ParenthesizedExpression(expr);
    }

    private Expression ParseCall(Expression left)
    {
        var arguments = new List<Expression>();
        while (Current.Type != TokenType.CloseParen)
        {
            do
            {
                arguments.Add(ParseExpression());
            } while (Match(TokenType.Comma));
        }
            
        Consume(TokenType.CloseParen);
                
        return new CallExpression(left, [..arguments]);
    }

    private bool IsEndOfFile => Current.Type == TokenType.Eof; 
    
    private Token Previous => _tokens[_position - 1];
    private Token Current  => _tokens[_position];

    private ParseRule CurrentParseRule =>
        _rules.TryGetValue(Current.Type, out var parseRule)
            ? parseRule
            : throw new InvalidOperationException($"Could not parse token {Current.Type}");

    private Precedence CurrentPrecedence =>
        _rules.TryGetValue(Current.Type, out var parseRule)
            ? parseRule.Precedence
            : Precedence.None;

    private bool Match(TokenType expected)
    {
        if (Current.Type != expected)
            return false;
        
        _position++;
        return true;
    }
    
    private void Consume() => _position++;
    
    private void Consume(TokenType expected)
    {
        if (Current.Type != expected)
            throw new InvalidOperationException($"Expected {expected} but got {Current.Type}");

        _position++;
    }
}

internal record SyntaxTree(List<Statement> Statements);

internal abstract record Statement;
internal sealed record FunctionDeclarationStatement(Token Identifier, Parameter[] Parameters, IdentifierType ReturnValue, BlockStatement Body) : Statement;
internal sealed record ExpressionStatement(Expression Expression) : Statement;
internal sealed record BindingDeclarationStatement(Token Identifier, Expression Value) : Statement;
internal sealed record BlockStatement(Statement[] Statements) : Statement;

internal sealed record IdentifierType(Token Identifier)
{
    public Type RuntimeType => Identifier.Type switch
    {
        TokenType.IntKeyword => typeof(int),
        TokenType.BoolKeyword => typeof(bool),
        _ => throw new InvalidOperationException($"Unexpected token {Identifier}")
    };
}

internal sealed record Parameter(Token Identifier, IdentifierType IdentifierType);

internal abstract record Expression;
internal sealed record LiteralExpression(Token Value) : Expression;
internal sealed record NameExpression(Token Identifier) : Expression;
internal sealed record CallExpression(Expression Function, Expression[] Arguments) : Expression;
internal sealed record UnaryExpression(Token Operator, Expression Value) : Expression;
internal sealed record BinaryExpression(Expression Left, Token Operator, Expression Right) : Expression;
internal sealed record ParenthesizedExpression(Expression Expression) : Expression;
internal sealed record LogicalAndExpression(Expression Left, Expression Right) : Expression;
internal sealed record LogicalOrExpression(Expression Left, Expression Right) : Expression;

internal sealed record MatchExpression(Expression Input, MatchCase[] Cases) : Expression;
internal sealed record MatchCase(MatchPattern Pattern, Expression ReturnValue);
internal abstract record MatchPattern;
internal sealed record ConstantMatchPattern(Token Value) : MatchPattern;
internal sealed record NegationMatchPattern(Token Value) : MatchPattern;
internal sealed record BindingMatchPattern(Token Identifier) :  MatchPattern;
internal sealed record ComparisonMatchPattern(Token Operator, Token CompareValue) :  MatchPattern;
internal sealed record DiscardPattern :  MatchPattern;
