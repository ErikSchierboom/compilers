namespace BoomScript;

public enum SyntaxKind
{
    // Tokens
    NumberToken,
    IdentifierToken,
    PlusToken,
    MinusToken,
    MinusGreaterThanToken,
    StarToken,
    SlashToken,
    EqualsToken,
    CommaToken,
    ColonToken,
    OpenParenthesisToken,
    CloseParenthesisToken,
    OpenBraceToken,
    CloseBraceToken,
    NewlineToken,
    EndOfFileToken,
    
    // Keywords
    FnKeyword,
    
    // Nodes
    CompilationUnit,
    
    // Expressions
    BinaryExpression,
    LiteralExpression,
    ParenthesizedExpression,
    
    // Statements
    ExpressionStatement,
    FunctionDeclarationStatement,
    BlockStatement,
    VariableDeclarationStatement,
    
    // Syntax
    ParameterSyntax,
    TypeClauseSyntax,
}

public class SyntaxTree
{
    public SourceText SourceText { get; }
    public CompilationUnitSyntax Root { get; }
    
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

public sealed record CompilationUnitSyntax(Statement[] Statements, SyntaxTree Tree, TextSpan Span) : SyntaxNode(Tree, SyntaxKind.CompilationUnit, Span)
{
    public override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor) => visitor.VisitCompilationUnitSyntax(this);
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

public sealed record BlockStatement(Statement[] Statements, SyntaxTree Tree, TextSpan Span) : Statement(Tree, SyntaxKind.BlockStatement, Span)
{
    public override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor) => visitor.VisitBlockStatement(this);
}

public sealed record FunctionDeclarationStatement(
    SyntaxToken Identifier,
    ParameterSyntax[] Parameters,
    TypeClauseSyntax ReturnType,
    BlockStatement Body,
    SyntaxTree Tree,
    TextSpan Span) : Statement(Tree, SyntaxKind.FunctionDeclarationStatement, Span)
{
    public override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor) => visitor.VisitFunctionDeclarationStatement(this);
}

public sealed record ParameterSyntax(SyntaxToken Name, TypeClauseSyntax Type, SyntaxTree Tree, TextSpan Span)
    : SyntaxNode(Tree, SyntaxKind.ParameterSyntax, Span)
{
    public override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor) => visitor.VisitParameterSyntax(this);
}

public sealed record TypeClauseSyntax(SyntaxToken Type, SyntaxTree Tree, TextSpan Span)
    : SyntaxNode(Tree, SyntaxKind.TypeClauseSyntax, Span)
{
    public override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor) => visitor.VisitTypeClauseSyntax(this);
}

public abstract class SyntaxVisitor<TResult>
{
    public TResult Visit(SyntaxNode element) => element.Accept(this);

    public virtual TResult VisitCompilationUnitSyntax(CompilationUnitSyntax node) => Visit(node);
    public virtual TResult VisitTypeClauseSyntax(TypeClauseSyntax node) => Visit(node);
    public virtual TResult VisitParameterSyntax(ParameterSyntax node) => Visit(node);

    public virtual TResult VisitUnaryExpression(UnaryExpression node) => Visit(node);
    public virtual TResult VisitBinaryExpression(BinaryExpression node) => Visit(node);
    public virtual TResult VisitLiteralExpression(LiteralExpression node) => Visit(node);
    public virtual TResult VisitParenthesizedExpression(ParenthesizedExpression node) => Visit(node);
    public virtual TResult VisitNameExpression(NameExpression node) => Visit(node);
    public virtual TResult VisitCallExpression(CallExpression node) => Visit(node);

    public virtual TResult VisitExpressionStatement(ExpressionStatement node) => Visit(node);
    public virtual TResult VisitVariableDeclarationStatement(VariableDeclarationStatement node) => Visit(node);
    public virtual TResult VisitFunctionDeclarationStatement(FunctionDeclarationStatement node) => Visit(node);
    public virtual TResult VisitBlockStatement(BlockStatement node) => Visit(node);
}
