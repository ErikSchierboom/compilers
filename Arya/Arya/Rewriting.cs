namespace Arya;

public abstract class ExpressionVisitor
{
    public void Visit(Expression expression)
    {
        switch (expression)
        {
            case ArrayExpression arrayExpression:
                VisitArray(arrayExpression);
                break;
            case LambdaExpression lambdaExpression:
                VisitLambda(lambdaExpression);
                break;
            case AssignmentExpression assignmentExpression:
                VisitAssignment(assignmentExpression);
                break;
            case BinaryExpression binaryExpression:
                VisitBinary(binaryExpression);
                break;
            case BlockExpression blockExpression:
                VisitBlock(blockExpression);
                break;
            case BoxExpression boxExpression:
                VisitBox(boxExpression);
                break;
            case CallExpression callExpression:
                VisitCall(callExpression);
                break;
            case LiteralExpression literalExpression:
                VisitLiteral(literalExpression);
                break;
            case NameExpression nameExpression:
                VisitName(nameExpression);
                break;
            case PlaceholderExpression nameExpression:
                VisitPlaceholder(nameExpression);
                break;
            case ParenthesizedExpression parenthesizedExpression:
                VisitParenthesized(parenthesizedExpression);
                break;
            case UnaryExpression unaryExpression:
                VisitUnary(unaryExpression);
                break;
            default:
                throw new ArgumentOutOfRangeException(nameof(expression));
        }
    }

    protected virtual void VisitArray(ArrayExpression arrayExpression)
    {
        foreach (var element in arrayExpression.Elements)
            Visit(element);
    }

    protected virtual void VisitLambda(LambdaExpression lambdaExpression) =>
        Visit(lambdaExpression.Body);

    protected virtual void VisitAssignment(AssignmentExpression assignmentExpression)
    {
        VisitName(assignmentExpression.Identifier);
        Visit(assignmentExpression.Value);
    }

    protected virtual void VisitBinary(BinaryExpression binaryExpression)
    {
        Visit(binaryExpression.Left);
        Visit(binaryExpression.Right);
    }

    protected virtual void VisitBlock(BlockExpression blockExpression)
    {
        foreach (var expression in blockExpression.Expressions)
            Visit(expression);
    }

    protected virtual void VisitBox(BoxExpression boxExpression) =>
        Visit(boxExpression.Expression);

    protected virtual void VisitCall(CallExpression callExpression)
    {
        foreach (var arg in callExpression.Arguments)
            Visit(arg);
    }

    protected virtual void VisitLiteral(LiteralExpression literalExpression)
    {
    }

    protected virtual void VisitName(NameExpression nameExpression)
    {
    }

    protected virtual void VisitPlaceholder(PlaceholderExpression placeholderExpression)
    {
    }

    protected virtual void VisitParenthesized(ParenthesizedExpression parenthesizedExpression) =>
        Visit(parenthesizedExpression.Expression);

    protected virtual void VisitUnary(UnaryExpression unaryExpression) =>
        Visit(unaryExpression.Operand);
}

public abstract class ExpressionRewriter
{
    public Expression Rewrite(Expression expression) =>
        expression switch
        {
            ArrayExpression arrayExpression => RewriteArray(arrayExpression),
            LambdaExpression lambdaExpression => RewriteLambda(lambdaExpression),
            AssignmentExpression assignmentExpression => RewriteAssignment(assignmentExpression),
            BinaryExpression binaryExpression => RewriteBinary(binaryExpression),
            BlockExpression blockExpression => RewriteBlock(blockExpression),
            BoxExpression boxExpression => RewriteBox(boxExpression),
            CallExpression callExpression => RewriteCall(callExpression),
            LiteralExpression literalExpression => RewriteLiteral(literalExpression),
            NameExpression nameExpression => RewriteName(nameExpression),
            PlaceholderExpression nameExpression => RewritePlaceholder(nameExpression),
            ParenthesizedExpression parenthesizedExpression => RewriteParenthesized(parenthesizedExpression),
            UnaryExpression unaryExpression => RewriteUnary(unaryExpression),
            _ => throw new ArgumentOutOfRangeException(nameof(expression))
        };

    protected virtual Expression RewriteArray(ArrayExpression arrayExpression) =>
        new ArrayExpression([..arrayExpression.Elements.Select(Rewrite)]);

    protected virtual Expression RewriteLambda(LambdaExpression lambdaExpression) =>
        new LambdaExpression(Rewrite(lambdaExpression.Body));

    protected virtual Expression RewriteAssignment(AssignmentExpression assignmentExpression)
    {
        var rewrittenIdentifier = RewriteName(assignmentExpression.Identifier);
        if (rewrittenIdentifier is not NameExpression rewrittenIdentifierName)
            throw new InvalidOperationException("Assignment expression's identifier must be a name expressions");

        return new AssignmentExpression(rewrittenIdentifierName, Rewrite(assignmentExpression.Value));
    }

    protected virtual Expression RewriteBinary(BinaryExpression binaryExpression) =>
        new BinaryExpression(
            Rewrite(binaryExpression.Left),
            binaryExpression.Operator,
            Rewrite(binaryExpression.Right));

    protected virtual Expression RewriteBlock(BlockExpression blockExpression) =>
        new BlockExpression([..blockExpression.Expressions.Select(Rewrite)]);

    protected virtual Expression RewriteBox(BoxExpression boxExpression) =>
        new BoxExpression(Rewrite(boxExpression.Expression));

    protected virtual Expression RewriteCall(CallExpression callExpression) =>
        new CallExpression(callExpression.FunctionName, [..callExpression.Arguments.Select(Rewrite)]);

    protected virtual Expression RewriteLiteral(LiteralExpression literalExpression) => literalExpression;

    protected virtual Expression RewriteName(NameExpression nameExpression) => nameExpression;

    protected virtual Expression RewritePlaceholder(PlaceholderExpression placeholderExpression) => placeholderExpression;

    protected virtual Expression RewriteParenthesized(ParenthesizedExpression parenthesizedExpression) =>
        new ParenthesizedExpression(Rewrite(parenthesizedExpression.Expression));

    protected virtual Expression RewriteUnary(UnaryExpression unaryExpression) =>
        new UnaryExpression(unaryExpression.Operator, Rewrite(unaryExpression.Operand));
}

internal static class Lowerer
{
    public static readonly ExpressionRewriter[] _lowerers =
    [
        new ConstantFoldingLowerer(),
        new OperatorToFunctionLowerer(),
    ];

    public static Expression Lower(Expression expression) =>
        _lowerers.Aggregate(expression, (loweredExpression, lowerer) => lowerer.Rewrite(loweredExpression));

    private class OperatorToFunctionLowerer : ExpressionRewriter
    {
        protected override Expression RewriteUnary(UnaryExpression unaryExpression)
        {
            switch (unaryExpression.Operator.Type)
            {
                case TokenType.Plus:
                    return new CallExpression(new Token(TokenType.Identifier, "plus"), [unaryExpression.Operand]);
                case TokenType.Minus:
                    return new CallExpression(new Token(TokenType.Identifier, "minus"), [unaryExpression.Operand]);
                case TokenType.Exclamation:
                    return new CallExpression(new Token(TokenType.Identifier, "not"), [unaryExpression.Operand]);
            }

            return base.RewriteUnary(unaryExpression);
        }

        protected override Expression RewriteBinary(BinaryExpression binaryExpression)
        {
            switch (binaryExpression.Operator.Type)
            {
                case TokenType.Plus:
                    return new CallExpression(new Token(TokenType.Identifier, "add"), [binaryExpression.Left, binaryExpression.Right]);
                case TokenType.Minus:
                    return new CallExpression(new Token(TokenType.Identifier, "subtract"), [binaryExpression.Left, binaryExpression.Right]);
                case TokenType.Star:
                    return new CallExpression(new Token(TokenType.Identifier, "multiply"), [binaryExpression.Left, binaryExpression.Right]);
                case TokenType.Slash:
                    return new CallExpression(new Token(TokenType.Identifier, "divide"), [binaryExpression.Left, binaryExpression.Right]);
                case TokenType.Percent:
                    return new CallExpression(new Token(TokenType.Identifier, "modulo"), [binaryExpression.Left, binaryExpression.Right]);
                case TokenType.Ampersand:
                    return new CallExpression(new Token(TokenType.Identifier, "and"), [binaryExpression.Left, binaryExpression.Right]);
                case TokenType.Pipe:
                    return new CallExpression(new Token(TokenType.Identifier, "or"), [binaryExpression.Left, binaryExpression.Right]);
                case TokenType.GreaterGreater:
                    return new CallExpression(new Token(TokenType.Identifier, "shiftRight"), [binaryExpression.Left, binaryExpression.Right]);
                case TokenType.LessLess:
                    return new CallExpression(new Token(TokenType.Identifier, "shiftLeft"), [binaryExpression.Left, binaryExpression.Right]);
                case TokenType.PlusPlus:
                    return new CallExpression(new Token(TokenType.Identifier, "append"), [binaryExpression.Left, binaryExpression.Right]);
            }

            return base.RewriteBinary(binaryExpression);
        }
    }

    private class ConstantFoldingLowerer : ExpressionRewriter
    {
        protected override Expression RewriteUnary(UnaryExpression unaryExpression)
        {
            switch (unaryExpression.Operator.Type, unaryExpression.Operand)
            {
                case (TokenType.Plus, LiteralExpression { Value.Literal: int }):
                    return unaryExpression.Operand;
                case (TokenType.Minus, LiteralExpression { Value.Literal: int intVal }):
                    return new LiteralExpression(new Token(TokenType.Number, $"-{intVal}", -intVal));
            }

            return base.RewriteUnary(unaryExpression);
        }

        protected override Expression RewriteBinary(BinaryExpression binaryExpression)
        {
            switch (binaryExpression.Left, binaryExpression.Operator.Type, binaryExpression.Right)
            {
                case (LiteralExpression left, TokenType.Plus, LiteralExpression right):
                    switch (left.Value.Literal, right.Value.Literal)
                    {
                        case (0, _):
                            return right;
                        case (_, 0):
                            return left;
                        case (int leftInt, int rightInt):
                            return new LiteralExpression(new Token(TokenType.Number, $"{leftInt + rightInt}", leftInt + rightInt));
                    }
                    break;
                case (LiteralExpression left, TokenType.Minus, LiteralExpression right):
                    switch (left.Value.Literal, right.Value.Literal)
                    {
                        case (0, int rightVal):
                            return new LiteralExpression(new Token(TokenType.Number, $"-{rightVal}", -rightVal));
                        case (_, 0):
                            return left;
                        case (int leftInt, int rightInt):
                            return new LiteralExpression(new Token(TokenType.Number, $"{leftInt - rightInt}", leftInt - rightInt));
                    }
                    break;
                case (LiteralExpression left, TokenType.Star, LiteralExpression right):
                    switch (left.Value.Literal, right.Value.Literal)
                    {
                        case (0, _) or (_, 0):
                            return new LiteralExpression(new Token(TokenType.Number, "0", 0));
                        case (1, _):
                            return right;
                        case (_, 1):
                            return left;
                        case (int leftInt, int rightInt):
                            return new LiteralExpression(new Token(TokenType.Number, $"{leftInt * rightInt}", leftInt * rightInt));
                    }
                    break;
                case (LiteralExpression left, TokenType.Slash, LiteralExpression right):
                    switch (left.Value.Literal, right.Value.Literal)
                    {
                        case (_, 1):
                            return left;
                        case (int leftInt, int rightInt):
                            return new LiteralExpression(new Token(TokenType.Number, $"{leftInt / rightInt}", leftInt / rightInt));
                    }
                    break;
            }

            return base.RewriteBinary(binaryExpression);
        }
    }
}
