namespace BoomScript;

public abstract record Expression(TextSpan Span);

public sealed record IntegerExpression(int Value, TextSpan Span) : Expression(Span);

public sealed record NameExpression(string Identifier, TextSpan Span) : Expression(Span);

public sealed record BinaryExpression(Expression Left, BinaryOperatorKind Operator, Expression Right, TextSpan Span) : Expression(Span);

public sealed record AssignmentExpression(string Identifier, Expression Value, TextSpan Span) : Expression(Span);

public enum BinaryOperatorKind
{
    Plus
}

public sealed class Parser
{
    private enum Precedence
    {
        None,
        Sum
    }
    
    private delegate Expression? UnaryParser();
    private delegate Expression BinaryParser(Expression left);
    private record ParseRule(UnaryParser? ParseUnary, BinaryParser? ParseBinary, Precedence Precedence);
    
    private readonly SourceText _sourceText;
    private readonly Dictionary<TokenKind, ParseRule> _parseRules;

    public Parser(SourceText sourceText)
    {
        _sourceText = sourceText;
        _parseRules = new Dictionary<TokenKind, ParseRule>
        {
            [TokenKind.Number] = new(ParseIntegerExpression, null, Precedence.None),
            [TokenKind.Identifier] = new(ParseNameExpression, null, Precedence.None)
        };
    }
    
    private Expression[] Parse()
    {
        var tokens = Lexer.Lex(_sourceText);
        throw new NotImplementedException();
    }
    
    public static Expression[] Parse(SourceText sourceText) => new Parser(sourceText).Parse();
    
    private Expression ParseIntegerExpression()
    {
        // TODO: use token
        throw new NotImplementedException();
    }
    
    private Expression ParseNameExpression()
    {
        // TODO: use token
        throw new NotImplementedException();
    }
}