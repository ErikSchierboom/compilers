namespace Arya;

internal static class Lowerer
{
    private static readonly ExpressionRewriter[] _lowerers =
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
