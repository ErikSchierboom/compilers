namespace Gleamy;

internal class Parser(List<Token> tokens)
{
    private int _position = 0;
    
    public SyntaxTree Parse()
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
        if (Match(TokenType.IntKeyword))
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

    private Expression ParseExpression()
    {   
        return ParseEqualityExpression();
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
            var input = ParseTermExpression();
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
        
        return ParseTermExpression();
    }

    private MatchCase ParseMatchCase()
    {
        var pattern = ParseMatchPattern();
        Consume(TokenType.EqualGreater);
        var returnValue = ParseTermExpression();
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
            return new ComparisonMatchPattern(operatorToken, compareValue);
        }
        
        throw new InvalidOperationException($"Unexpected token {Previous}");
    }

    private Expression ParseTermExpression()
    {
        var left = ParseFactorExpression();
        
        while (Match(TokenType.Plus) || Match(TokenType.Minus))
            left = new BinaryExpression(left,  Previous, ParseFactorExpression());

        return left;
    }

    private Expression ParseFactorExpression()
    {
        var left = ParseUnaryExpression();
        
        while (Match(TokenType.Star) || Match(TokenType.Slash))
            left = new BinaryExpression(left,  Previous, ParseUnaryExpression());

        return left;
    }

    private Expression ParseUnaryExpression()
    {
        if (Match(TokenType.Minus) || Match(TokenType.Plus))
            return new UnaryExpression(Previous, ParseUnaryExpression());

        return ParsePrimaryExpression();
    }

    private Expression ParsePrimaryExpression()
    {
        if (Match(TokenType.Number))
            return new LiteralExpression(Previous.Literal!);
        
        if (Match(TokenType.TrueKeyword) || Match(TokenType.FalseKeyword))
            return new LiteralExpression(Previous.Literal!);

        if (Match(TokenType.Identifier))
        {
            var identifier = Previous;

            if (!Match(TokenType.OpenParen))
                return new NameExpression(identifier);
            
            var arguments = new List<Expression>();
            while (!Check(TokenType.CloseParen))
            {
                do
                {
                    arguments.Add(ParseExpression());
                } while (Match(TokenType.Comma));
            }
            
            Consume(TokenType.CloseParen);
                
            return new CallExpression(identifier, [..arguments]);
        }
        
        throw new InvalidOperationException($"Unexpected token {Previous}");
    }

    private bool IsEndOfFile => Current.Type == TokenType.Eof; 
    
    private Token Previous => tokens[_position - 1];
    private Token Current  => tokens[_position];

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

internal sealed record IdentifierType(Token Identifier);
internal sealed record Parameter(Token Identifier, IdentifierType IdentifierType);

internal abstract record Expression;
internal sealed record LiteralExpression(object Value) : Expression;
internal sealed record NameExpression(Token Identifier) : Expression;
internal sealed record CallExpression(Token Identifier, Expression[] Arguments) : Expression;
internal sealed record UnaryExpression(Token Operator, Expression Value) : Expression;
internal sealed record BinaryExpression(Expression Left, Token Operator, Expression Right) : Expression;

internal sealed record MatchExpression(Expression Input, MatchCase[] Cases) : Expression;
internal sealed record MatchCase(MatchPattern Pattern, Expression ReturnValue);
internal abstract record MatchPattern;
internal sealed record ConstantMatchPattern(Token Value) : MatchPattern;
internal sealed record BindingMatchPattern(Token Identifier) :  MatchPattern;
internal sealed record ComparisonMatchPattern(Token Operator, Expression CompareValue) :  MatchPattern;
internal sealed record DiscardPattern :  MatchPattern;
