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
        private static readonly Dictionary<TokenType, string> _unaryTokenTypeToFunctionName = new()
        {
            [TokenType.Plus] = "plus",
            [TokenType.Minus] = "minus",
            [TokenType.Bang] = "not"
        };

        private static readonly Dictionary<TokenType, string> _binaryTokenTypeToFunctionName = new()
        {
            [TokenType.Plus] = "add",
            [TokenType.Minus] = "subtract",
            [TokenType.Star] = "multiply",
            [TokenType.Slash] = "divide",
            [TokenType.Percent] = "modulo",
            [TokenType.Ampersand] = "and",
            [TokenType.Pipe] = "or",
            [TokenType.Greater] = "greater",
            [TokenType.GreaterEqual] = "greaterEqual",
            [TokenType.GreaterGreater] = "shiftRight",
            [TokenType.Less] = "less",
            [TokenType.LessEqual] = "lessEqual",
            [TokenType.LessLess] = "shiftLeft",
            [TokenType.EqualEqual] = "equal",
            [TokenType.BangEqual] = "notEqual",
            [TokenType.PlusPlus] = "append"
        };

        protected override Expression RewriteUnary(UnaryExpression unaryExpression)
        {
            var operand = Rewrite(unaryExpression.Operand);

            if (_unaryTokenTypeToFunctionName.TryGetValue(unaryExpression.Operator.Type, out var functionName))
                return new CallExpression(new NameExpression(new Token(TokenType.Identifier, functionName)), [operand], []);

            return base.RewriteUnary(unaryExpression);
        }

        protected override Expression RewriteBinary(BinaryExpression binaryExpression)
        {
            var left = Rewrite(binaryExpression.Left);
            var right = Rewrite(binaryExpression.Right);

            if (_binaryTokenTypeToFunctionName.TryGetValue(binaryExpression.Operator.Type, out var functionName))
                return new CallExpression(new NameExpression(new Token(TokenType.Identifier, functionName)), [left, right], []);

            return base.RewriteBinary(binaryExpression);
        }
    }

    private class ConstantFoldingLowerer : ExpressionRewriter
    {
        protected override Expression RewriteUnary(UnaryExpression unaryExpression) =>
            (unaryExpression.Operator.Type, unaryExpression.Operand) switch
            {
                (TokenType.Plus, LiteralExpression { Value.Literal: int }) => unaryExpression.Operand,
                (TokenType.Minus, LiteralExpression { Value.Literal: int intVal }) => new LiteralExpression(new Token(TokenType.Number, $"-{intVal}", -intVal)),
                _ => base.RewriteUnary(unaryExpression)
            };

        protected override Expression RewriteBinary(BinaryExpression binaryExpression) =>
            (binaryExpression.Left, binaryExpression.Operator.Type, binaryExpression.Right) switch
            {
                (LiteralExpression left, TokenType.Plus, LiteralExpression right) => (left.Value.Literal, right.Value.Literal) switch
                {
                    (0, _) => right,
                    (_, 0) => left,
                    (int leftInt, int rightInt) => new LiteralExpression(new Token(TokenType.Number, $"{leftInt + rightInt}", leftInt + rightInt)),
                    _ => base.RewriteBinary(binaryExpression)
                },
                (LiteralExpression left, TokenType.Minus, LiteralExpression right) => (left.Value.Literal, right.Value.Literal) switch
                {
                    (0, int rightVal) => new LiteralExpression(new Token(TokenType.Number, $"-{rightVal}", -rightVal)),
                    (_, 0) => left,
                    (int leftInt, int rightInt) => new LiteralExpression(new Token(TokenType.Number, $"{leftInt - rightInt}", leftInt - rightInt)),
                    _ => base.RewriteBinary(binaryExpression)
                },
                (LiteralExpression left, TokenType.Star, LiteralExpression right) => (left.Value.Literal, right.Value.Literal) switch
                {
                    (0, _) or (_, 0) => new LiteralExpression(new Token(TokenType.Number, "0", 0)),
                    (1, _) => right,
                    (_, 1) => left,
                    (int leftInt, int rightInt) => new LiteralExpression(new Token(TokenType.Number, $"{leftInt * rightInt}", leftInt * rightInt)),
                    _ => base.RewriteBinary(binaryExpression)
                },
                (LiteralExpression left, TokenType.Slash, LiteralExpression right) => (left.Value.Literal, right.Value.Literal) switch
                {
                    (_, 1) => left,
                    (int leftInt, int rightInt) => new LiteralExpression(new Token(TokenType.Number, $"{leftInt / rightInt}", leftInt / rightInt)),
                    _ => base.RewriteBinary(binaryExpression)
                },
                _ => base.RewriteBinary(binaryExpression)
            };
    }
}
