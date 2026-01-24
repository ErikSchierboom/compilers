namespace BoomScript;

public record SyntaxTree(SourceText SourceText);

public enum SyntaxKind
{
    // Tokens
    NumberToken,
    IdentifierToken,
    PlusToken,
    MinusToken,
    StarToken,
    SlashToken,
    EqualsToken,
    OpenParenthesisToken,
    CloseParenthesisToken,
    NewlineToken,
    EndOfFileToken,
    
    // Nodes
    CompilationUnit,
    
    // Expressions
    BinaryExpression,
    LiteralExpression,
    ParenthesizedExpression,
    
    // Statements
    ExpressionStatement,
    VariableDeclarationStatement,
}

public abstract record SyntaxElement(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span)
{
    public TextLocation Location => field ??= new TextLocation(Tree.SourceText, Span);   
    public string Text => field ??= Tree.SourceText.Text.Substring(Span.Start, Span.Length);

    public override string ToString() => Text;
}

public sealed record SyntaxToken(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span) : SyntaxElement(Tree, Kind, Span);
public abstract record SyntaxNode(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span) : SyntaxElement(Tree, Kind, Span);

public sealed record CompilationUnit(Statement[] Statements, SyntaxToken EndOfFileToken, SyntaxTree Tree, TextSpan Span) : SyntaxNode(Tree, SyntaxKind.CompilationUnit, Span);

public abstract record Expression(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span): SyntaxNode(Tree, Kind, Span);
public sealed record UnaryExpression(SyntaxToken OperatorToken, Expression Operand, SyntaxTree Tree, TextSpan Span) : Expression(Tree, SyntaxKind.BinaryExpression, Span);
public sealed record BinaryExpression(Expression Left, SyntaxToken OperatorToken, Expression Right, SyntaxTree Tree, TextSpan Span) : Expression(Tree, SyntaxKind.BinaryExpression, Span);
public sealed record LiteralExpression(SyntaxToken LiteralToken, object? Value, SyntaxTree Tree, TextSpan Span) : Expression(Tree, SyntaxKind.LiteralExpression, Span);
public sealed record ParenthesizedExpression(SyntaxToken OpenParenthesisToken, Expression Expression, SyntaxToken ClosesParenthesisToken, SyntaxTree Tree, TextSpan Span) : Expression(Tree, SyntaxKind.ParenthesizedExpression, Span);
public sealed record NameExpression(SyntaxToken IdentifierToken, SyntaxTree Tree, TextSpan Span) : Expression(Tree, SyntaxKind.ParenthesizedExpression, Span);
public sealed record CallExpression(Expression Name, SyntaxToken OperParenthesisToken, SyntaxToken ClosesParenthesisToken, SyntaxTree Tree, TextSpan Span) : Expression(Tree, SyntaxKind.ParenthesizedExpression, Span);

public abstract record Statement(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span): SyntaxNode(Tree, Kind, Span);
public sealed record ExpressionStatement(Expression Expression, SyntaxTree Tree, TextSpan Span) : Statement(Tree, SyntaxKind.ExpressionStatement, Span);
public sealed record VariableDeclarationStatement(SyntaxToken Identifier, SyntaxToken EqualsToken, Expression Initializer, SyntaxTree Tree, TextSpan Span) : Statement(Tree, SyntaxKind.VariableDeclarationStatement, Span);
