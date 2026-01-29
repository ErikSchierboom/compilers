namespace BoomScript;

public enum SymbolKind
{
    Function,
    Variable,
    Parameter,
    Type,
}

public abstract record Symbol(string Name, SymbolKind Kind);

public record TypeSymbol(string Name) : Symbol(Name, SymbolKind.Type);
public record VariableSymbol(string Name, TypeSymbol Type) : Symbol(Name, SymbolKind.Variable);
public record ParameterSymbol(string Name, TypeSymbol Type, int Ordinal) : Symbol(Name, SymbolKind.Parameter);
public record FunctionSymbol(string Name, ParameterSymbol[] Parameters, TypeSymbol ReturnType) : Symbol(Name, SymbolKind.Function);


public enum BoundNodeKind
{
    // Statements
    BlockStatement,
    VariableDeclaration,
    ExpressionStatement,

    // Expressions
    LiteralExpression,
    VariableExpression,
    AssignmentExpression,
    UnaryExpression,
    BinaryExpression,
    CallExpression,
}

public abstract record BoundNode(SyntaxNode Node, BoundNodeKind Kind);
public abstract record BoundExpression(TypeSymbol Type, SyntaxNode Node, BoundNodeKind Kind) : BoundNode(Node, Kind);