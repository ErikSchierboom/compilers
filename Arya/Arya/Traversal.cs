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
            case KeywordExpression keywordExpression:
                VisitKeyword(keywordExpression);
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

    protected virtual void VisitLambda(LambdaExpression lambdaExpression)
    {
        foreach (var parameter in lambdaExpression.Parameters)
            Visit(parameter);

        Visit(lambdaExpression.Body);
    }

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
        Visit(callExpression.Target);

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

    protected virtual void VisitKeyword(KeywordExpression keywordExpression)
    {
        VisitName(keywordExpression.Identifier);
        Visit(keywordExpression.Value);
    }
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
            KeywordExpression keywordExpression => RewriteKeyword(keywordExpression),
            _ => throw new ArgumentOutOfRangeException(nameof(expression))
        };

    protected virtual Expression RewriteArray(ArrayExpression arrayExpression) =>
        new ArrayExpression([..arrayExpression.Elements.Select(Rewrite)]);

    protected virtual Expression RewriteLambda(LambdaExpression lambdaExpression) =>
        new LambdaExpression(Rewrite(lambdaExpression.Body), [..lambdaExpression.Parameters.Select(Rewrite).Cast<NameExpression>()]);

    protected virtual Expression RewriteAssignment(AssignmentExpression assignmentExpression) =>
        new AssignmentExpression((NameExpression)RewriteName(assignmentExpression.Identifier), Rewrite(assignmentExpression.Value));

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
        new CallExpression(
            Rewrite(callExpression.Target),
            [.. callExpression.Arguments.Select(Rewrite)],
            [.. callExpression.Keywords.Select(Rewrite).Cast<KeywordExpression>()]);

    protected virtual Expression RewriteKeyword(KeywordExpression keywordExpression) =>
        new KeywordExpression((NameExpression)RewriteName(keywordExpression.Identifier), Rewrite(keywordExpression.Value));

    protected virtual Expression RewriteLiteral(LiteralExpression literalExpression) => literalExpression;

    protected virtual Expression RewriteName(NameExpression nameExpression) => nameExpression;

    protected virtual Expression RewritePlaceholder(PlaceholderExpression placeholderExpression) => placeholderExpression;

    protected virtual Expression RewriteParenthesized(ParenthesizedExpression parenthesizedExpression) =>
        new ParenthesizedExpression(Rewrite(parenthesizedExpression.Expression));

    protected virtual Expression RewriteUnary(UnaryExpression unaryExpression) =>
        new UnaryExpression(unaryExpression.Operator, Rewrite(unaryExpression.Operand));
}
