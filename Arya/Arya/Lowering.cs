namespace Arya;

internal static class Lowerer
{
    private static ExpressionRewriter[] Lowerers => [new ConstantFoldingLowerer(), new OperatorToFunctionLowerer(), new LambdaPlaceholderLowerer()];

    public static Expression Lower(Expression expression) =>
        Lowerers.Aggregate(expression, (loweredExpression, lowerer) => lowerer.Rewrite(loweredExpression));

    private class LambdaPlaceholderLowerer : ExpressionRewriter
    {
        private readonly Stack<SortedSet<int>> _placeholdersStack = new();

        protected override Expression RewriteLambda(LambdaExpression lambdaExpression)
        {
            _placeholdersStack.Push([]);

            base.RewriteLambda(lambdaExpression);

            var placeholders = _placeholdersStack.Pop();

            if (placeholders.Count == 0)
                return lambdaExpression;

            if (lambdaExpression.Parameters.Length > 0)
                throw new InvalidOperationException("Lambda placeholder lowerer should not be used on lambda expressions with parameters");

            if (placeholders.Count > 1 && placeholders.Contains(0))
                throw new InvalidOperationException("Lambda placeholder lowerer should not be used on lambda expressions with multiple placeholders");

            if (placeholders.Count == 1 && placeholders.Contains(0))
                return lambdaExpression with { Parameters = [new NameExpression(new Token(TokenType.Identifier, "#"))] };

            var parameters = placeholders.Select(index => new NameExpression(new Token(TokenType.Identifier, $"#{index}")));
            return lambdaExpression with { Parameters = [.. parameters] };
        }

        protected override Expression RewritePlaceholder(PlaceholderExpression placeholderExpression)
        {
            _placeholdersStack.Peek().Add((int)placeholderExpression.Identifier.Literal!);
            return placeholderExpression;
        }
    }

    private class OperatorToFunctionLowerer : ExpressionRewriter
    {
        protected override Expression RewriteUnary(UnaryExpression unaryExpression)
        {
            var operand = Rewrite(unaryExpression.Operand);

            switch (unaryExpression.Operator.Type)
            {
                case TokenType.Plus:
                    return new CallExpression(new NameExpression(new Token(TokenType.Identifier, "plus")), [operand]);
                case TokenType.Minus:
                    return new CallExpression(new NameExpression(new Token(TokenType.Identifier, "minus")), [operand]);
                case TokenType.Exclamation:
                    return new CallExpression(new NameExpression(new Token(TokenType.Identifier, "not")), [operand]);
            }

            return base.RewriteUnary(unaryExpression);
        }

        protected override Expression RewriteBinary(BinaryExpression binaryExpression)
        {
            var left = Rewrite(binaryExpression.Left);
            var right = Rewrite(binaryExpression.Right);

            switch (binaryExpression.Operator.Type)
            {
                case TokenType.Plus:
                    return new CallExpression(new NameExpression(new Token(TokenType.Identifier, "add")), [left, right]);
                case TokenType.Minus:
                    return new CallExpression(new NameExpression(new Token(TokenType.Identifier, "subtract")), [left, right]);
                case TokenType.Star:
                    return new CallExpression(new NameExpression(new Token(TokenType.Identifier, "multiply")), [left, right]);
                case TokenType.Slash:
                    return new CallExpression(new NameExpression(new Token(TokenType.Identifier, "divide")), [left, right]);
                case TokenType.Percent:
                    return new CallExpression(new NameExpression(new Token(TokenType.Identifier, "modulo")), [left, right]);
                case TokenType.Ampersand:
                    return new CallExpression(new NameExpression(new Token(TokenType.Identifier, "and")), [left, right]);
                case TokenType.Pipe:
                    return new CallExpression(new NameExpression(new Token(TokenType.Identifier, "or")), [left, right]);
                case TokenType.GreaterGreater:
                    return new CallExpression(new NameExpression(new Token(TokenType.Identifier, "shiftRight")), [left, right]);
                case TokenType.LessLess:
                    return new CallExpression(new NameExpression(new Token(TokenType.Identifier, "shiftLeft")), [left, right]);
                case TokenType.PlusPlus:
                    return new CallExpression(new NameExpression(new Token(TokenType.Identifier, "append")), [left, right]);
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
