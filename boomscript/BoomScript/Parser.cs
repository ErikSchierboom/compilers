namespace BoomScript;

public abstract record Expression(TextSpan Span);

public sealed record IntegerExpression(int Value, TextSpan Span) : Expression(Span);

public sealed record NameExpression(string Identifier, TextSpan Span) : Expression(Span);

public sealed record BinaryExpression(Expression Left, Token Operator, Expression Right, TextSpan Span) : Expression(Span);

public sealed record AssignmentExpression(string Identifier, Expression Value, TextSpan Span) : Expression(Span);

public sealed class Parser
{
    private enum Precedence
    {
        None,
        Assignment,
        Sum,
        Product
    }
    
    private delegate Expression ParsePrefix();
    private delegate Expression ParseInfix(Expression left);
    private record ParseRule(ParsePrefix? UnaryParser, ParseInfix? InfixParser, Precedence Precedence);
    
    private readonly SourceText _sourceText;
    private readonly Dictionary<TokenKind, ParseRule> _parseRules;
    private Token[] _tokens;
    private int _position;

    public Parser(SourceText sourceText)
    {
        _sourceText = sourceText;
        _parseRules = new Dictionary<TokenKind, ParseRule>
        {
            [TokenKind.Number] = new(ParseIntegerExpression, null, Precedence.None),
            [TokenKind.Identifier] = new(ParseNameExpression, null, Precedence.None),
            [TokenKind.Equals] = new(null, ParseAssignmentExpression, Precedence.Assignment),
            [TokenKind.Plus] = new(null, ParseBinaryExpression, Precedence.Sum),
            [TokenKind.Star] = new(null, ParseBinaryExpression, Precedence.Product),
        };
    }
    
    public static Expression[] Parse(SourceText sourceText) => new Parser(sourceText).Parse();
    
    private Expression[] Parse()
    {
        _tokens = Lexer.Lex(_sourceText);

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

    private ParseRule GetParseRule(Token token) 
    {
        if (_parseRules.TryGetValue(token.Kind, out var rule))
            return rule;
            
        return new ParseRule(null, null, Precedence.None);
    }

    private Expression ParseIntegerExpression()
    {
        var token = Expect(TokenKind.Number);
        return new IntegerExpression(int.Parse(_sourceText[token.Span]), token.Span);
    }
    
    private Expression ParseNameExpression()
    {
        var token = Expect(TokenKind.Identifier);
        return new NameExpression(_sourceText[token.Span], token.Span);
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
        if (left is not NameExpression name)
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