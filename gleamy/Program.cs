using Gleamy;

const string code = """
                    fn cube (x: Int) -> Int {
                        let y = x * x;
                        y * x
                    }
                    """;

var tokens = new Scanner(code).Scan();
var tree = new Parser(tokens).Parse();

foreach (var statement in tree.Statements)
{
    Console.WriteLine(statement);
}

internal class Parser(List<Token> tokens)
{
    private int _position = 0;
    
    public SyntaxTree Parse()
    {
        var statements = new List<StatementSyntax>();

        while (!IsEndOfFile)
            statements.Add(ParseStatement());

        return new SyntaxTree(statements);
    }

    private StatementSyntax ParseStatement()
    {
        if (Match(TokenType.LetKeyword))
            return ParseBindingDeclarationStatement();

        if (Match(TokenType.FnKeyword))
            return ParseFunctionDeclarationStatement();
        
        return ParseExpressionStatement();
    }

    private BindingDeclarationStatementSyntax ParseBindingDeclarationStatement()
    {
        Consume(TokenType.Identifier);
        var identifier = Previous;
        Consume(TokenType.Equal);
        var expression = ParseExpression();
        return new BindingDeclarationStatementSyntax(identifier, expression);
    }

    private FunctionDeclarationStatementSyntax ParseFunctionDeclarationStatement()
    {
        Consume(TokenType.Identifier);
        var identifier = Previous;

        Consume(TokenType.OpenParen);
        var parameters = new List<ParameterSyntax>();
        while (true)
        {
            parameters.Add(ParseParameter());

            if (!Match(TokenType.Comma))
                break;
        }
        Consume(TokenType.CloseParen);
        
        Consume(TokenType.MinusGreaterThan);
        var returnType = ParseType();
        
        var body = ParseBlockStatement();
        
        return new FunctionDeclarationStatementSyntax(identifier, parameters, returnType, body);
    }

    private BlockStatementSyntax ParseBlockStatement()
    {
        Consume(TokenType.OpenBracket);
        
        var statements = new List<StatementSyntax>();
        while (true)
        {
            statements.Add(ParseStatement());

            if (!Match(TokenType.Semicolon))
                break;
        }
         
        Consume(TokenType.CloseBracket);
        
        return new BlockStatementSyntax(statements);
    }

    private TypeSyntax ParseType()
    {
        if (Match(TokenType.IntKeyword))
            return new TypeSyntax(Previous);
        
        throw new InvalidOperationException($"Expected type but got {Current.Type}");
    }

    private ParameterSyntax ParseParameter()
    {
        Consume(TokenType.Identifier);
        var identifier = Previous;
        Consume(TokenType.Colon);
        var type = ParseType();
        return new ParameterSyntax(identifier, type);
    }

    private ExpressionStatementSyntax ParseExpressionStatement()
    {
        var expression = ParseExpression();
        return new ExpressionStatementSyntax(expression);
    }

    private ExpressionSyntax ParseExpression()
    {
        return ParseTermExpression();
    }

    private ExpressionSyntax ParseTermExpression()
    {
        var left = ParseFactorExpression();
        
        while (Match(TokenType.Plus) || Match(TokenType.Minus))
            left = new BinaryExpressionSyntax(left,  Previous, ParseFactorExpression());

        return left;
    }

    private ExpressionSyntax ParseFactorExpression()
    {
        var left = ParsePrimaryExpression();
        
        while (Match(TokenType.Star) || Match(TokenType.Slash))
            left = new BinaryExpressionSyntax(left,  Previous, ParsePrimaryExpression());

        return left;
    }

    private ExpressionSyntax ParsePrimaryExpression()
    {
        if (Match(TokenType.Number))
            return new NumericLiteralExpressionSyntax(Previous);
        
        if (Match(TokenType.Identifier))
            return new IdentifierNameExpressionSyntax(Previous);
        
        throw new InvalidOperationException($"Unexpected token {Previous}");
    }

    private bool IsEndOfFile => Current.Type == TokenType.Eof; 
    
    private Token Current => tokens[_position];
    private Token Previous => tokens[_position - 1];

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
}

internal record SyntaxTree(List<StatementSyntax> Statements);

internal abstract record StatementSyntax;
internal sealed record FunctionDeclarationStatementSyntax(Token Identifier, List<ParameterSyntax> Parameters, TypeSyntax ReturnValue, BlockStatementSyntax Body) : StatementSyntax;
internal sealed record ExpressionStatementSyntax(ExpressionSyntax Value) : StatementSyntax;
internal sealed record BindingDeclarationStatementSyntax(Token Identifier, ExpressionSyntax Value) : StatementSyntax;
internal sealed record BlockStatementSyntax(List<StatementSyntax> Statements) : StatementSyntax;

internal sealed record TypeSyntax(Token Identifier);
internal sealed record ParameterSyntax(Token Identifier, TypeSyntax Type);

internal abstract record ExpressionSyntax;
internal sealed record NumericLiteralExpressionSyntax(Token Value) : ExpressionSyntax;
internal sealed record IdentifierNameExpressionSyntax(Token Identifier) : ExpressionSyntax;
internal sealed record BinaryExpressionSyntax(ExpressionSyntax Left, Token Operator, ExpressionSyntax Right) : ExpressionSyntax;
