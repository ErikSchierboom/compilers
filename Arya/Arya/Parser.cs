namespace Arya;

internal enum Precedence
{
    None,         // =
    Array,        // []
    Addition,     // + -
    Product,      // *
    Unary,        // -
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
            [TokenType.Plus] = new(ParseUnary, ParseBinary, Precedence.Addition),
            [TokenType.PlusPlus] = new(null, ParseBinary, Precedence.Addition),
            [TokenType.Minus] = new(ParseUnary, ParseBinary, Precedence.Addition),
            [TokenType.Star] = new(null, ParseBinary, Precedence.Product),
            [TokenType.Slash] = new(null, ParseBinary, Precedence.Product),
            [TokenType.Percent] = new(null, ParseBinary, Precedence.Product),
            [TokenType.Number] = new(ParseLiteral, null, Precedence.Primary),
            [TokenType.String] = new(ParseLiteral, null, Precedence.Primary),
            [TokenType.Char] = new(ParseLiteral, null, Precedence.Primary),
            [TokenType.Identifier] = new(ParseName, null, Precedence.Primary),
            [TokenType.OpenBracket] = new(ParseArray, ParseIndexer, Precedence.Array),
            [TokenType.OpenParen] = new(ParseParenthesized, ParseCall, Precedence.Call),
        };
    }

    public static List<Expression> Parse(string source)
    {
        var tokens = Scanner.Scan(source);
        return new Parser(tokens).Parse();
    }

    private List<Expression> Parse()
    {
        var expressions = new List<Expression>();

        while (!IsEndOfFile)
            expressions.Add(ParseExpression());

        return expressions;
    }

    private Expression ParseExpression(Precedence precedence = Precedence.None)
    {
        var parsePrefixFn = CurrentParseRule.Prefix ?? throw new InvalidOperationException("Expected prefix");
        Consume();
        var left = parsePrefixFn();
        
        while (precedence < CurrentPrecedence)
        {
            var parseInfixFn = CurrentParseRule.Infix;
            if (parseInfixFn is null)
                break;
            
            Consume();
            left = parseInfixFn(left);
        }

        return left;
    }
    
    private UnaryExpression ParseUnary() => new(Previous, ParseExpression(Precedence.Unary));

    private BinaryExpression ParseBinary(Expression left)
    {
        var operatorToken = Previous;
        var rule = _rules[operatorToken.Type];
        var right = ParseExpression(rule.Precedence);

        return new BinaryExpression(left, operatorToken, right);
    }

    private LiteralExpression ParseLiteral() => new(Previous);
    
    private NameExpression ParseName() => new(Previous);

    private IndexerExpression ParseIndexer(Expression left)
    {
        var index = ParseExpression();
        Consume(TokenType.CloseBracket);
        return new IndexerExpression(left, index);
    }
    
    private Expression ParseCall(Expression left)
    {
        if (left is not NameExpression name)
            throw new InvalidOperationException("Can only call names");
        
        var arguments = new List<Expression>();
        while (!IsEndOfFile)
        {
            if (Match(TokenType.CloseParen))
                break;
            
            do
            {
                arguments.Add(ParseExpression());
            } while (Match(TokenType.Comma));
        }
                
        return new CallExpression(name.Identifier, [..arguments]);
    }

    private ArrayExpression ParseArray()
    {
        var elements = new List<Expression>();
        while (!IsEndOfFile && Current.Type != TokenType.CloseBracket)
            elements.Add(ParseExpression(Precedence.Addition));

        Consume(TokenType.CloseBracket);
        
        return new([..elements]);
    }
    
    private ParenthesizedExpression ParseParenthesized()
    {
        var expr = ParseExpression();
        Consume(TokenType.CloseParen);
        return new(expr);
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

internal abstract record Expression;
internal sealed record LiteralExpression(Token Value) : Expression;
internal sealed record ArrayExpression(Expression[] Elements) : Expression;
internal sealed record NameExpression(Token Identifier) : Expression;
internal sealed record CallExpression(Token FunctionName, Expression[] Arguments) : Expression;
internal sealed record UnaryExpression(Token Operator, Expression Operand) : Expression;
internal sealed record BinaryExpression(Expression Left, Token Operator, Expression Right) : Expression;
internal sealed record ParenthesizedExpression(Expression Expression) : Expression;
internal sealed record IndexerExpression(Expression Target, Expression Index) : Expression;