namespace BoomScript;

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
    CommaToken,
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

public class SyntaxTree
{
    public SourceText SourceText { get; }
    public CompilationUnit Root { get; }
    
    private SyntaxTree(SourceText sourceText)
    {
        SourceText = sourceText;
        Root = new Parser(this).ParseCompilationUnit();
    }
    
    public static SyntaxTree Parse(string text) => new(new SourceText(text));
}

public abstract record SyntaxElement(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span)
{
    public TextLocation Location => field ??= new TextLocation(Tree.SourceText, Span);   
    public string Text => field ??= Tree.SourceText.Text.Substring(Span.Start, Span.Length);

    public override string ToString() => Text;
}

public sealed record SyntaxToken(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span) : SyntaxElement(Tree, Kind, Span);

public abstract record SyntaxNode(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span) : SyntaxElement(Tree, Kind, Span)
{
    public abstract TResult Accept<TResult>(SyntaxVisitor<TResult> visitor);
}

public sealed record CompilationUnit(Statement[] Statements, SyntaxTree Tree, TextSpan Span) : SyntaxNode(Tree, SyntaxKind.CompilationUnit, Span)
{
    public override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor) => visitor.VisitCompilationUnit(this);
}

public abstract record Expression(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span): SyntaxNode(Tree, Kind, Span);
public sealed record UnaryExpression(SyntaxToken OperatorToken, Expression Operand, SyntaxTree Tree, TextSpan Span) : Expression(Tree, SyntaxKind.BinaryExpression, Span)
{
    public override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor) => visitor.VisitUnaryExpression(this);
}

public sealed record BinaryExpression(Expression Left, SyntaxToken OperatorToken, Expression Right, SyntaxTree Tree, TextSpan Span) : Expression(Tree, SyntaxKind.BinaryExpression, Span)
{
    public override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor) => visitor.VisitBinaryExpression(this);
}

public sealed record LiteralExpression(SyntaxToken LiteralToken, object? Value, SyntaxTree Tree, TextSpan Span) : Expression(Tree, SyntaxKind.LiteralExpression, Span)
{
    public override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor) => visitor.VisitLiteralExpression(this);
}

public sealed record ParenthesizedExpression(Expression Expression, SyntaxTree Tree, TextSpan Span) : Expression(Tree, SyntaxKind.ParenthesizedExpression, Span)
{
    public override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor) => visitor.VisitParenthesizedExpression(this);
}

public sealed record NameExpression(SyntaxToken IdentifierToken, SyntaxTree Tree, TextSpan Span) : Expression(Tree, SyntaxKind.ParenthesizedExpression, Span)
{
    public override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor) => visitor.VisitNameExpression(this);
}

public sealed record CallExpression(Expression Name, Expression[] Arguments, SyntaxTree Tree, TextSpan Span) : Expression(Tree, SyntaxKind.ParenthesizedExpression, Span)
{
    public override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor) => visitor.VisitCallExpression(this);
}

public abstract record Statement(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span): SyntaxNode(Tree, Kind, Span);
public sealed record ExpressionStatement(Expression Expression, SyntaxTree Tree, TextSpan Span) : Statement(Tree, SyntaxKind.ExpressionStatement, Span)
{
    public override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor) => visitor.VisitExpressionStatement(this);
}

public sealed record VariableDeclarationStatement(SyntaxToken Identifier, Expression Initializer, SyntaxTree Tree, TextSpan Span) : Statement(Tree, SyntaxKind.VariableDeclarationStatement, Span)
{
    public override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor) => visitor.VisitVariableDeclarationStatement(this);
}

public abstract class SyntaxVisitor<TResult>
{
    public TResult Visit(SyntaxNode element) => element.Accept(this);

    public virtual TResult VisitCompilationUnit(CompilationUnit node) => Visit(node);

    public virtual TResult VisitUnaryExpression(UnaryExpression node) => Visit(node);
    public virtual TResult VisitBinaryExpression(BinaryExpression node) => Visit(node);
    public virtual TResult VisitLiteralExpression(LiteralExpression node) => Visit(node);
    public virtual TResult VisitParenthesizedExpression(ParenthesizedExpression node) => Visit(node);
    public virtual TResult VisitNameExpression(NameExpression node) => Visit(node);
    public virtual TResult VisitCallExpression(CallExpression node) => Visit(node);

    public virtual TResult VisitExpressionStatement(ExpressionStatement node) => Visit(node);
    public virtual TResult VisitVariableDeclarationStatement(VariableDeclarationStatement node) => Visit(node);
}
