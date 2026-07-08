namespace Gleamy;

internal enum Precedence
{
    PREC_NONE,
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_ADDITION,    // + -
    PREC_PRODUCT,     // * /
    PREC_UNARY,       // ! -
    PREC_CALL,        // ()
    PREC_PRIMARY
}

internal delegate Expression ParsePrefixFn();
internal delegate Expression ParseInfixFn(Expression left);

internal record ParseRule(ParsePrefixFn? Prefix, ParseInfixFn? Infix, Precedence Precedence);

internal class Parser(List<Token> tokens)
{
    private int _position = 0;

    private static readonly Dictionary<TokenType, ParseRule> _rules = new()
    {
        [TokenType.Eof] = new(null, null, Precedence.PREC_NONE)
    };
    
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
        return new BindingDeclarationStatement(identifier, expression);
    }

    private FunctionDeclarationStatement ParseFunctionDeclarationStatement()
    {
        Consume(TokenType.Identifier);
        var identifier = Previous;

        Consume(TokenType.OpenParen);
        var parameters = new List<Parameter>();
        while (true)
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
        while (true)
        {
            statements.Add(ParseStatement());

            if (!Match(TokenType.Semicolon))
                break;
        }
         
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
        return new ExpressionStatement(expression);
    }

    private Expression ParseExpression(Precedence precedence = Precedence.PREC_NONE)
    {
        var parsePrefix = _rules[Current.Type].Prefix;
        Advance();

        if (parsePrefix is null)
            throw new InvalidOperationException("Expect prefix");

        var left = parsePrefix();
        
        while (precedence <= _rules[Current.Type].Precedence)
        {
            Advance();
            var parseInfix = _rules[Previous.Type].Infix;
            if (parseInfix is null)
                throw new InvalidOperationException("Expect infix");
                
            left = parseInfix(left);
        }

        return left;
    }
    
    private Expression ParseLogicalOrExpression(Expression left)
    {   
        while (Match(TokenType.PipePipe))
            left = new LogicalOrExpression(left, ParseLogicalAndExpression());

        return left;
    }
    
    private Expression ParseLogicalAndExpression()
    {
        var left = ParseBitwiseOrExpression();
        
        while (Match(TokenType.AmpersandAmpersand))
            left = new LogicalAndExpression(left, ParseBitwiseOrExpression());

        return left;
    }
    
    private Expression ParseBinaryExpression(Expression left)
    {
        var operatorToken = Previous;
        var rule = _rules[operatorToken.Type];
        var right = ParseExpression(rule.Precedence + 1);

        return new BinaryExpression(left, operatorToken, right);
    }
    
    private Expression ParseBitwiseAndExpression()
    {
        var left = ParseEqualityExpression();
        
        while (Match(TokenType.Ampersand))
            left = new BinaryExpression(left, Previous, ParseEqualityExpression());

        return left;
    }
    
    private Expression ParseEqualityExpression()
    {
        Expression left = ParseComparisonExpression();
        
        while (Match(TokenType.EqualEqual) || Match(TokenType.BangEqual))
            left = new BinaryExpression(left,  Previous, ParseComparisonExpression());

        return left;
    }
    
    private Expression ParseComparisonExpression()
    {
        Expression left = ParseMatchExpression();
        
        while (Match(TokenType.Greater) || Match(TokenType.GreaterEqual) ||
               Match(TokenType.Less) || Match(TokenType.LessEqual))
            left = new BinaryExpression(left,  Previous, ParseMatchExpression());

        return left;
    }

    private Expression ParseMatchExpression()
    {
        if (Match(TokenType.MatchKeyword))
        {
            var input = ParseAdditiveExpression();
            Consume(TokenType.OpenBracket);
        
            var cases = new List<MatchCase>();
        
            while (!Check(TokenType.CloseBracket))
            {
                do
                {
                    cases.Add(ParseMatchCase());
                } while (Match(TokenType.Comma));
            }
            
            Consume(TokenType.CloseBracket);
        
            return new MatchExpression(input, [..cases]);
        }
        
        return ParseAdditiveExpression();
    }

    private MatchCase ParseMatchCase()
    {
        var pattern = ParseMatchPattern();
        Consume(TokenType.EqualGreater);
        var returnValue = ParseAdditiveExpression();
        return new MatchCase(pattern, returnValue);
    }

    private MatchPattern ParseMatchPattern()
    {
        if (Match(TokenType.Underscore))
            return new DiscardPattern();

        if (Match(TokenType.Identifier))
            return new BindingMatchPattern(Previous);

        if (Match(TokenType.Number))
            return new ConstantMatchPattern(Previous);

        if (Match(TokenType.Greater) || Match(TokenType.GreaterEqual) ||
            Match(TokenType.Less) || Match(TokenType.LessEqual))
        {
            var operatorToken = Previous;
            var compareValue = ParseUnaryExpression();
            if (compareValue is not LiteralExpression literal)
                throw new InvalidOperationException($"Unexpected token {compareValue}");

            return new ComparisonMatchPattern(operatorToken, literal.Value);
        }
    }

    private Expression ParseUnaryExpression() => new UnaryExpression(Previous, ParseExpression());

    private Expression ParseNumber() => new LiteralExpression(Previous);
    private Expression ParseBoolean() => new LiteralExpression(Previous);
    private Expression ParseName() => new LiteralExpression(Previous);
    private Expression ParseParenthesized()
    {
        var expr = ParseExpression();
        Consume(TokenType.CloseParen);
        return new ParenthesizedExpression(expr);
    }

    private Expression ParseCall(Expression left)
    {
        var arguments = new List<Expression>();
        while (!Check(TokenType.CloseParen))
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
    
    private Token Previous => tokens[_position - 1];
    private Token Current  => tokens[_position];

    private void Advance()
    {
        _position++;
    }

    private bool Match(TokenType expected)
    {
        if (Current.Type != expected)
            return false;
        
        _position++;
        return true;
    }
    
    private void Consume(TokenType expected)
    {
        if (Current.Type != expected)
            throw new InvalidOperationException($"Expected {expected} but got {Current.Type}");
        
        _position++;
    }

    private bool Check(TokenType expected)
    {
        return Current.Type == expected;
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
internal sealed record BindingMatchPattern(Token Identifier) :  MatchPattern;
internal sealed record ComparisonMatchPattern(Token Operator, Token CompareValue) :  MatchPattern;
internal sealed record DiscardPattern :  MatchPattern;
