namespace Gleamy;

internal static class BuiltinFunctions
{
    public static readonly FunctionSymbol Abs = new("abs", TypeSymbol.Int, [new ParameterSymbol("x", TypeSymbol.Int)], null);
}

public class Interpreter
{
    private readonly BoundProgram _program;

    private Interpreter(BoundProgram program) => _program = program;

    private static readonly Frame _defaultFrame = new()
    {
        ["abs"] = BuiltinFunctions.Abs,
    };
    
    public static object? Evaluate(string source)
    {
        var tree = Parser.Parse(source);
        var program = Binder.Bind(tree);
        return new Interpreter(program).Evaluate(_defaultFrame.CreateChild());
    }

    private object? Evaluate(Frame frame)
    {
        object? result = null;   
        
        foreach (var statement in _program.Statements)
            result = Evaluate(statement, frame);

        return result;
    }

    private object? Evaluate(BoundStatement statement, Frame frame) =>
        statement switch
        {
            BoundBindingDeclarationStatement bindingDeclarationStatement => Evaluate(bindingDeclarationStatement, frame),
            BoundBlockStatement blockStatement => Evaluate(blockStatement, frame),
            BoundExpressionStatement expressionStatement => Evaluate(expressionStatement, frame),
            BoundFunctionDeclarationStatement functionDeclarationStatement => Evaluate(functionDeclarationStatement, frame),
            _ => throw new ArgumentOutOfRangeException(nameof(statement))
        };

    private object? Evaluate(BoundBindingDeclarationStatement bindingDeclarationStatement, Frame frame)
    {
        var value = Evaluate(bindingDeclarationStatement.Value, frame);
        frame[bindingDeclarationStatement.Binding.Name] = value;
        return null;
    }

    private object? Evaluate(BoundFunctionDeclarationStatement functionDeclarationStatement, Frame frame)
    {
        frame[functionDeclarationStatement.Function.Name] = functionDeclarationStatement.Body;
        return null;
    }
    
    private object? Evaluate(BoundBlockStatement blockStatement, Frame frame)
    {
        object? result = null;
        
        foreach (var statement in blockStatement.Statements)
            result = Evaluate(statement, frame);

        return result;
    }
    
    private object? Evaluate(BoundExpressionStatement expressionStatement, Frame frame) =>
        Evaluate(expressionStatement.Expression, frame);

    private object? Evaluate(BoundExpression expression, Frame frame) =>
        expression switch
        {
            BoundUnaryExpression unaryExpression => Evaluate(unaryExpression, frame),
            BoundBinaryExpression binaryExpression => Evaluate(binaryExpression, frame),
            BoundCallExpression callExpression => Evaluate(callExpression, frame),
            BoundLiteralExpression literalExpression => Evaluate(literalExpression, frame),
            BoundNameExpression nameExpression => Evaluate(nameExpression, frame),
            BoundValueMatchExpression valueMatchExpression => Evaluate(valueMatchExpression, frame),
            BoundExpressionMatchExpression expressionMatchExpression => Evaluate(expressionMatchExpression, frame),
            BoundParenthesizedExpression parenthesizedExpression => Evaluate(parenthesizedExpression, frame),
            BoundLogicalAndExpression andExpression => Evaluate(andExpression, frame),
            BoundLogicalOrExpression orExpression => Evaluate(orExpression, frame),
            _ => throw new ArgumentOutOfRangeException(nameof(expression))
        };

    private object? Evaluate(BoundCallExpression callExpression, Frame frame)
    {
        if (callExpression.Function.Parameters.Count != callExpression.Arguments.Count)
            throw new ArgumentException("Invalid number of arguments");

        var functionFrame = frame.CreateChild();

        foreach (var (argument, parameter) in callExpression.Arguments.Zip(callExpression.Function.Parameters))
        {
            if (argument.Type != parameter.Type)
                throw new InvalidOperationException("Invalid type of argument");
            
            var argumentValue = Evaluate(argument, functionFrame);
            functionFrame[parameter.Name] = argumentValue;
        }

        if (callExpression.Function == BuiltinFunctions.Abs)
            return Math.Abs((int)functionFrame[BuiltinFunctions.Abs.Parameters[0].Name]!);

        var body = (BoundBlockStatement)frame[callExpression.Function.Name]!;
        return Evaluate(body, functionFrame);
    }

    private object? Evaluate(BoundNameExpression nameExpression, Frame frame) =>
        frame[nameExpression.Symbol.Name];

    private object Evaluate(BoundLiteralExpression literalExpression, Frame frame) =>
        literalExpression.Value.Literal!;

    private object? Evaluate(BoundParenthesizedExpression parenthesizedExpression, Frame frame) => 
        Evaluate(parenthesizedExpression.Expression, frame);

    private object? Evaluate(BoundLogicalAndExpression logicalAndExpression, Frame frame)
    {
        var left = Evaluate(logicalAndExpression.Left, frame) ?? throw new InvalidOperationException("Cannot apply && to null");
        if (left is not bool leftBool)
            throw new InvalidOperationException("Cannot apply && to non-boolean");

        if (!leftBool)
            return false;
        
        var right = Evaluate(logicalAndExpression.Right, frame) ?? throw new InvalidOperationException("Cannot apply && to null");;
        if (right is not bool rightBool)
            throw new InvalidOperationException("Cannot apply && to non-boolean");

        return rightBool;
    }

    private object? Evaluate(BoundLogicalOrExpression logicalOrExpression, Frame frame)
    {
        var left = Evaluate(logicalOrExpression.Left, frame) ?? throw new InvalidOperationException("Cannot apply && to null");
        if (left is not bool leftBool)
            throw new InvalidOperationException("Cannot apply && to non-boolean");

        if (leftBool)
            return true;
        
        var right = Evaluate(logicalOrExpression.Right, frame) ?? throw new InvalidOperationException("Cannot apply && to null");;
        if (right is not bool rightBool)
            throw new InvalidOperationException("Cannot apply && to non-boolean");

        return rightBool;
    }

    private object? Evaluate(BoundBinaryExpression binaryExpression, Frame frame)
    {
        var left =  Evaluate(binaryExpression.Left, frame) ?? throw new InvalidOperationException("Cannot apply binary operation to null");
        var right = Evaluate(binaryExpression.Right, frame) ?? throw new InvalidOperationException("Cannot apply binary operation to null");;

        return (binaryExpression.Operator.Kind, left, right) switch
        {
            (BoundBinaryOperatorKind.Addition, int l, int r) => l + r,
            (BoundBinaryOperatorKind.Subtraction, int l, int r) => l - r,
            (BoundBinaryOperatorKind.Multiplication, int l, int r) => l * r,
            (BoundBinaryOperatorKind.Division, int l, int r) => l / r,
            (BoundBinaryOperatorKind.Modulus, int l, int r) => l % r,
            (BoundBinaryOperatorKind.BitwiseAnd, int l, int r) => l & r,
            (BoundBinaryOperatorKind.BitwiseXor, int l, int r) => l ^ r,
            (BoundBinaryOperatorKind.BitwiseOr, int l, int r) => l | r,
            (BoundBinaryOperatorKind.Less, int l, int r) => l < r,
            (BoundBinaryOperatorKind.LeftShift, int l, int r) => l << r,
            (BoundBinaryOperatorKind.LessEqual, int l, int r) => l <= r,
            (BoundBinaryOperatorKind.Greater, int l, int r) => l > r,
            (BoundBinaryOperatorKind.RightShift, int l, int r) => l >> r,
            (BoundBinaryOperatorKind.GreaterEqual, int l, int r) => l >= r,
            (BoundBinaryOperatorKind.Equality, int l, int r) or => l == r,
            (BoundBinaryOperatorKind.Equality, bool l, bool r) => l == r,
            (BoundBinaryOperatorKind.Inequality, int l, int r) => l != r,
            (BoundBinaryOperatorKind.Inequality, bool l, bool r) => l != r,
            _ => throw new ArgumentOutOfRangeException(nameof(binaryExpression.Operator))
        };
    }

    private object? Evaluate(BoundUnaryExpression unaryExpression, Frame frame)
    {
        var value = Evaluate(unaryExpression.Value, frame) ?? throw new InvalidOperationException("Cannot apply unary operation to null");

        return (unaryExpression.Operator.Kind, value) switch
        {
            (BoundUnaryOperatorKind.Plus, int i) => i,
            (BoundUnaryOperatorKind.Minus, int i) => -i,
            (BoundUnaryOperatorKind.Negation, bool b) => !b,
            (BoundUnaryOperatorKind.Complement, int i) => ~i,
            _ => throw new ArgumentOutOfRangeException(nameof(unaryExpression.Operator))
        };
    }

    private object? Evaluate(BoundValueMatchExpression valueMatchExpression, Frame frame)
    {
        var input = Evaluate(valueMatchExpression.Input, frame);

        foreach (var matchCase in valueMatchExpression.Cases)
        {
            switch (matchCase.Pattern)
            {
                case BoundBindingValueMatchPattern bindingMatchPattern:
                    var bindingMatchFrame = frame.CreateChild();
                    bindingMatchFrame[bindingMatchPattern.Identifier.Text] = input;
            
                    return Evaluate(matchCase.ReturnValue, bindingMatchFrame);
                case BoundConstantValueMatchPattern constantMatchPattern:
                    switch (constantMatchPattern.Value.Value, input)
                    {
                        case (int intMatch, int intInput) when intInput == intMatch:
                        case (bool boolMatch, bool boolInput) when boolInput == boolMatch:
                            return Evaluate(matchCase.ReturnValue, frame);
                    }
                    break;
                case BoundNegationValueMatchPattern constantMatchPattern:
                    switch (constantMatchPattern.Value.Value, input)
                    {
                        case (int intMatch, int intInput) when intInput != intMatch:
                        case (bool boolMatch, bool boolInput) when boolInput != boolMatch:
                            return Evaluate(matchCase.ReturnValue, frame);
                    }
                    break;
                case BoundComparisonValueMatchPattern comparisonMatchPattern:
                    switch (comparisonMatchPattern.Operator.Type, comparisonMatchPattern.CompareValue.Value, input)
                    {
                        case (TokenType.Greater, int comparison1, int input1) when input1 > comparison1:
                        case (TokenType.GreaterEqual, int comparison2, int input2) when input2 > comparison2:
                        case (TokenType.Less, int comparison3, int input3) when input3 < comparison3:
                        case (TokenType.LessEqual, int comparison4, int input4) when input4 <= comparison4:
                        case (TokenType.EqualEqual, int comparison5, int input5) when input5 == comparison5:
                        case (TokenType.EqualEqual, bool comparison6, bool input6) when input6 == comparison6:
                        case (TokenType.BangEqual, int comparison7, int input7) when input7 == comparison7:
                        case (TokenType.BangEqual, bool comparison8, bool input8) when input8 != comparison8:
                            return Evaluate(matchCase.ReturnValue, frame);
                    }
                    break;
                case BoundDiscardValueMatchPattern:
                    return Evaluate(matchCase.ReturnValue, frame);
                default:
                    throw new ArgumentOutOfRangeException();
            }
        }

        throw new InvalidOperationException("No matching pattern found");
    }
    
    private object? Evaluate(BoundExpressionMatchExpression expressionMatchExpression, Frame frame)
    {
        foreach (var matchCase in expressionMatchExpression.Cases)
        {
            switch (matchCase.Pattern)
            {
                case BoundExpressionExpressionMatchPattern expressionExpressionMatchPattern:
                    var evaluatedExpression = Evaluate(expressionExpressionMatchPattern.Expression, frame);
                    if (evaluatedExpression is not bool b)
                        throw new InvalidOperationException("Can only evaluate boolean expressions");
                    
                    if (b)
                        return Evaluate(matchCase.ReturnValue, frame);

                    break;
                case BoundDiscardExpressionMatchPattern:
                    return Evaluate(matchCase.ReturnValue, frame);
                default:
                    throw new ArgumentOutOfRangeException();
            }
        }

        throw new InvalidOperationException("No matching pattern found");
    }
}

internal class Frame(Frame? parent = null)
{
    private readonly Dictionary<string, object?> _locals = new();

    public Frame CreateChild() => new(this);
        
    public object? this[string key]
    {
        get
        {
            if (_locals.TryGetValue(key, out var result))
                return result;
            
            return parent?[key];
        }
        set
        {
            if (!_locals.TryAdd(key, value))
                throw new InvalidOperationException("Cannot redeclare local");;
        }
    }
}
