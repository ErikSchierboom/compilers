namespace Arya;

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
            [TokenType.Ampersand] = new(null, ParseBinary, Precedence.BitwiseAnd),
            [TokenType.GreaterGreater] = new(null, ParseBinary, Precedence.BitwiseShift),
            [TokenType.LessLess] = new(null, ParseBinary, Precedence.BitwiseShift),
            [TokenType.Bang] = new(ParseUnary, null, Precedence.Unary),
            [TokenType.Number] = new(ParseLiteral, null, Precedence.Primary),
            [TokenType.String] = new(ParseLiteral, null, Precedence.Primary),
            [TokenType.Boolean] = new(ParseLiteral, null, Precedence.Primary),
            [TokenType.Char] = new(ParseLiteral, null, Precedence.Primary),
            [TokenType.Identifier] = new(ParseName, null, Precedence.Primary),
            [TokenType.Placeholder] = new(ParsePlaceholder, null, Precedence.Primary),
            [TokenType.Pipe] = new(null, ParseBinary, Precedence.BitwiseOr),
            [TokenType.Colon] = new(null, ParseKeyword, Precedence.Assignment),
            [TokenType.At] = new(ParseBox, null, Precedence.Unary),
            [TokenType.OpenBracket] = new(ParseArray, null, Precedence.Call),
            [TokenType.OpenParen] = new(ParseParenthesized, ParseCall, Precedence.Call),
            [TokenType.OpenBrace] = new(ParseLambda, null, Precedence.Call),
            [TokenType.Equal] = new(null, ParseAssignment, Precedence.Assignment),
            [TokenType.EqualEqual] = new(null, ParseBinary, Precedence.Equality),
            [TokenType.BangEqual] = new(null, ParseBinary, Precedence.Equality),
            [TokenType.Greater] = new(null, ParseBinary, Precedence.Comparison),
            [TokenType.GreaterEqual] = new(null, ParseBinary, Precedence.Comparison),
            [TokenType.Less] = new(null, ParseBinary, Precedence.Comparison),
            [TokenType.LessEqual] = new(null, ParseBinary, Precedence.Comparison),
        };
    }

    public static BlockExpression Parse(string source)
    {
        var tokens = Scanner.Scan(source);
        return new Parser(tokens).Parse();
    }

    private BlockExpression Parse()
    {
        var expressions = new List<Expression>();

        while (!IsEndOfFile)
            expressions.Add(ParseExpression());

        return new BlockExpression(expressions);
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

    private PlaceholderExpression ParsePlaceholder() => new(Previous);

    private AssignmentExpression ParseAssignment(Expression left)
    {
        if (left is not NameExpression name)
            throw new InvalidOperationException("Can only call names");

        var value = ParseExpression();
        return new AssignmentExpression(name, value);
    }

    private KeywordExpression ParseKeyword(Expression left)
    {
        if (left is not NameExpression name)
            throw new InvalidOperationException("Expected name");

        var value = ParseExpression();
        return new KeywordExpression(name, value);
    }

    private CallExpression ParseCall(Expression left)
    {
        if (left is not (NameExpression or LambdaExpression))
            throw new InvalidOperationException("Can only call names");

        var arguments = new List<Expression>();
        var keywords = new List<KeywordExpression>();

        while (!IsEndOfFile)
        {
            if (Match(TokenType.CloseParen))
                break;

            do
            {
                arguments.Add(ParseExpression());
            } while (Match(TokenType.Comma));

            if (!Match(TokenType.SemiColon))
                continue;

            do
            {
                if (ParseExpression() is not KeywordExpression keyword)
                    throw new InvalidOperationException("Expected keyword expression");

                keywords.Add(keyword);
            } while (Match(TokenType.Comma));
        }

        return new CallExpression(left, [.. arguments], [.. keywords]);
    }
    
    private BoxExpression ParseBox()
    {
        var expression = ParseExpression(Precedence.Unary);

        return new(expression);
    }

    private ArrayExpression ParseArray()
    {
        var elements = new List<Expression>();
        while (!IsEndOfFile && Current.Type != TokenType.CloseBracket)
            elements.Add(ParseExpression(Precedence.Addition));

        Consume(TokenType.CloseBracket);

        return new([.. elements]);
    }

    private LambdaExpression ParseLambda()
    {
        var parameters = new List<NameExpression>();
        var body = new List<Expression>();

        while (!IsEndOfFile && Current.Type != TokenType.CloseBrace)
        {
            if (Match(TokenType.MinusGreater))
            {
                if (body.Count == 0)
                    throw new InvalidOperationException("Lambda expression with named parameter must have at least one parameter.");

                parameters.AddRange(body.OfType<NameExpression>());

                if (parameters.Count != body.Count)
                    throw new InvalidOperationException("Lambda expression parameters must be names.");

                body.Clear();
            }

            body.Add(ParseExpression());
        }

        Consume(TokenType.CloseBrace);

        if (body.Count == 0)
            throw new InvalidOperationException("Lambda expression must have a body.");

        if (body.Count == 1)
            return new(body[0], [..parameters]);

        return new(new BlockExpression([.. body]), [..parameters]);
    }

    private ParenthesizedExpression ParseParenthesized()
    {
        var expr = ParseExpression();
        Consume(TokenType.CloseParen);
        return new(expr);
    }

    private bool IsEndOfFile => Current.Type == TokenType.Eof;

    private Token Previous => _tokens[_position - 1];
    private Token Current => _tokens[_position];

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

    private enum Precedence
    {
        None,
        Assignment,   // = ; :
        Addition,     // + -
        Product,      // *
        Unary,        // + - ! @
        BitwiseShift, // << >>
        Comparison,   // < <= > >=
        Equality,     // == !=
        BitwiseOr,    // |
        BitwiseAnd,   // &
        Call,         // () [] { }
        Primary
    }

    private delegate Expression ParsePrefix();
    private delegate Expression ParseInfix(Expression left);
    private record ParseRule(ParsePrefix? Prefix, ParseInfix? Infix, Precedence Precedence);
}

public abstract record Expression;
public sealed record BlockExpression(List<Expression> Expressions) : Expression;
public sealed record LiteralExpression(Token Value) : Expression;
public sealed record ArrayExpression(Expression[] Elements) : Expression;
public sealed record LambdaExpression(Expression Body, NameExpression[] Parameters) : Expression;
public sealed record BoxExpression(Expression Expression) : Expression;
public sealed record NameExpression(Token Identifier) : Expression;
public sealed record PlaceholderExpression(Token Identifier) : Expression;
public sealed record CallExpression(Expression Target, Expression[] Arguments, KeywordExpression[] Keywords) : Expression;
public sealed record UnaryExpression(Token Operator, Expression Operand) : Expression;
public sealed record BinaryExpression(Expression Left, Token Operator, Expression Right) : Expression;
public sealed record ParenthesizedExpression(Expression Expression) : Expression;
public sealed record AssignmentExpression(NameExpression Identifier, Expression Value) : Expression;
public sealed record KeywordExpression(NameExpression Identifier, Expression Value) : Expression;
