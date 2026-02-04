namespace BoomScript;

public abstract record Expression(TextSpan Span);

public sealed record LiteralExpression(object Value, TextSpan Span) : Expression(Span);

public sealed record VariableExpression(string Identifier, TextSpan Span) : Expression(Span);

public sealed record BinaryExpression(Expression Left, Token Operator, Expression Right, TextSpan Span) : Expression(Span);

public sealed record AssignmentExpression(string Identifier, Expression Value, TextSpan Span) : Expression(Span);

public sealed class Parser
{
    private enum Precedence
    {
        None,
        Assignment,
        Comparison,
        Term,
        Factor
    }
    
    private delegate Expression ParsePrefix();
    private delegate Expression ParseInfix(Expression left);
    private record ParseRule(ParsePrefix? UnaryParser, ParseInfix? InfixParser, Precedence Precedence);
    
    private readonly SourceText _sourceText;
    private readonly Dictionary<TokenKind, ParseRule> _parseRules;
    private readonly Token[] _tokens;
    private int _position;

    private Parser(SourceText sourceText)
    {
        _sourceText = sourceText;
        _tokens = Lexer.Lex(_sourceText);
        _parseRules = new Dictionary<TokenKind, ParseRule>
        {
            [TokenKind.EndOfFile] = new(null, null, Precedence.None),
            [TokenKind.Number] = new(ParseLiteralExpression, null, Precedence.None),
            [TokenKind.Identifier] = new(ParseVariableExpression, null, Precedence.None),
            [TokenKind.Equals] = new(null, ParseAssignmentExpression, Precedence.Assignment),
            [TokenKind.Greater] = new(null, ParseBinaryExpression, Precedence.Comparison),
            [TokenKind.Less] = new(null, ParseBinaryExpression, Precedence.Comparison),
            [TokenKind.Plus] = new(null, ParseBinaryExpression, Precedence.Term),
            [TokenKind.Star] = new(null, ParseBinaryExpression, Precedence.Factor),
        };
    }
    
    public static Expression[] Parse(SourceText sourceText) => new Parser(sourceText).Parse();
    
    private Expression[] Parse()
    {
        var expressions = new List<Expression>();
        
        while (Current.Kind != TokenKind.EndOfFile)
            expressions.Add(ParseExpression(Precedence.None));
        
        return expressions.ToArray();
    }
    
    private Expression ParseExpression(Precedence precedence)
    {
        var prefixRule = GetParseRule(Current).UnaryParser ?? throw new InvalidOperationException("Expect expression.");
        var expression = prefixRule();

        while (precedence < GetParseRule(Current).Precedence) {
            var infixRule = GetParseRule(Current).InfixParser;
            if (infixRule is null) break;
            expression = infixRule(expression);
        }

        return expression;
    }

    private ParseRule GetParseRule(Token token) => _parseRules[token.Kind];

    private Expression ParseLiteralExpression()
    {
        var token = Expect(TokenKind.Number);
        return new LiteralExpression(int.Parse(_sourceText[token.Span]), token.Span);
    }
    
    private Expression ParseVariableExpression()
    {
        var token = Expect(TokenKind.Identifier);
        return new VariableExpression(_sourceText[token.Span], token.Span);
    }
    
    private Expression ParseBinaryExpression(Expression left)
    {
        var operatorToken = Advance();
        var rule = GetParseRule(operatorToken);

        var right = ParseExpression(rule.Precedence);
        return new BinaryExpression(left, operatorToken, right, left.Span.Merge(right.Span));
    }
    
    private Expression ParseAssignmentExpression(Expression left)
    {
        if (left is not VariableExpression name)
            throw new InvalidOperationException("Expect identifier.");
            
        var identifier = name.Identifier;
        var operatorToken = Advance();
        var rule = GetParseRule(operatorToken);

        var right = ParseExpression(rule.Precedence - 1);
        return new AssignmentExpression(identifier, right, left.Span.Merge(right.Span));
    }

    private Token Expect(TokenKind expectedTokenKind)
    {
        if (Current.Kind != expectedTokenKind)
            throw new InvalidOperationException($"Expected {expectedTokenKind} but got {Current.Kind}");

        return Advance();
    }
    
    private Token Advance() => _tokens[_position++];
    private Token Current => _tokens[_position];
}