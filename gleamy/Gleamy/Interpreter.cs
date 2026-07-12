namespace Gleamy;

public class Interpreter
{
    private readonly SyntaxTree _tree;

    private Interpreter(SyntaxTree tree) => _tree = tree;

    private static readonly Frame _defaultFrame = new()
    {
        ["abs"] = new BuiltinFunction((args) => Math.Abs((int)args[0]!), [typeof(int)])
    };
    
    public static object? Evaluate(string source)
    {
        var tree = Parser.Parse(source);
        return new Interpreter(tree).Evaluate(_defaultFrame);
    }

    private object? Evaluate(Frame frame)
    {
        object? result = null;   
        
        foreach (var statement in _tree.Statements)
            result = Evaluate(statement, frame);

        return result;
    }

    private object? Evaluate(Statement statement, Frame frame) =>
        statement switch
        {
            BindingDeclarationStatement bindingDeclarationStatement => Evaluate(bindingDeclarationStatement, frame),
            BlockStatement blockStatement => Evaluate(blockStatement, frame),
            ExpressionStatement expressionStatement => Evaluate(expressionStatement, frame),
            FunctionDeclarationStatement functionDeclarationStatement => Evaluate(functionDeclarationStatement, frame),
            _ => throw new ArgumentOutOfRangeException(nameof(statement))
        };

    private object? Evaluate(BindingDeclarationStatement bindingDeclarationStatement, Frame frame)
    {
        var value = Evaluate(bindingDeclarationStatement.Value, frame);
        frame[bindingDeclarationStatement.Identifier.Text] = value;
        return null;
    }

    private object? Evaluate(FunctionDeclarationStatement functionDeclarationStatement, Frame frame)
    {
        var userDefinedFunction = new UserDefinedFunction(functionDeclarationStatement, frame.CreateChild());
        frame[functionDeclarationStatement.Identifier.Text] = userDefinedFunction;
        return null;
    }
    
    private object? Evaluate(BlockStatement blockStatement, Frame frame)
    {
        object? result = null;
        
        foreach (var statement in blockStatement.Statements)
            result = Evaluate(statement, frame);

        return result;
    }
    
    private object? Evaluate(ExpressionStatement expressionStatement, Frame frame) =>
        Evaluate(expressionStatement.Expression, frame);

    private object? Evaluate(Expression expression, Frame frame) =>
        expression switch
        {
            UnaryExpression unaryExpression => Evaluate(unaryExpression, frame),
            BinaryExpression binaryExpression => Evaluate(binaryExpression, frame),
            CallExpression callExpression => Evaluate(callExpression, frame),
            LiteralExpression literalExpression => Evaluate(literalExpression, frame),
            NameExpression nameExpression => Evaluate(nameExpression, frame),
            MatchExpression matchExpression => Evaluate(matchExpression, frame),
            ParenthesizedExpression parenthesizedExpression => Evaluate(parenthesizedExpression, frame),
            LogicalAndExpression andExpression => Evaluate(andExpression, frame),
            LogicalOrExpression orExpression => Evaluate(orExpression, frame),
            _ => throw new ArgumentOutOfRangeException(nameof(expression))
        };

    private object? Evaluate(CallExpression callExpression, Frame frame)
    {
        var binding = Evaluate(callExpression.Function, frame);
        if (binding is null)
            throw new InvalidOperationException("Could not find function");
        
        if (binding is not ICallable callable)
            throw new InvalidOperationException("Not callable");

        if (callable.Signature.Length != callExpression.Arguments.Length)
            throw new ArgumentException("Invalid number of arguments");

        var arguments = new List<object?>(capacity: callExpression.Arguments.Length);

        foreach (var (argument, parameterType) in callExpression.Arguments.Zip(callable.Signature))
        {
            var argumentValue = Evaluate(argument, frame);
            if (parameterType != argumentValue?.GetType())
                throw new InvalidOperationException("Invalid type of argument");
            
            arguments.Add(argumentValue);
        }
    
        return callable.Invoke(this, [..arguments]);
    }

    private object? Evaluate(NameExpression nameExpression, Frame frame) =>
        frame[nameExpression.Identifier.Text];

    private object Evaluate(LiteralExpression literalExpression, Frame frame) =>
        literalExpression.Value.Literal!;

    private object? Evaluate(ParenthesizedExpression parenthesizedExpression, Frame frame) => 
        Evaluate(parenthesizedExpression.Expression, frame);

    private object? Evaluate(LogicalAndExpression logicalAndExpression, Frame frame)
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

    private object? Evaluate(LogicalOrExpression logicalOrExpression, Frame frame)
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

    private object? Evaluate(BinaryExpression binaryExpression, Frame frame)
    {
        var left =  Evaluate(binaryExpression.Left, frame) ?? throw new InvalidOperationException("Cannot apply binary operation to null");
        var right = Evaluate(binaryExpression.Right, frame) ?? throw new InvalidOperationException("Cannot apply binary operation to null");;

        return (binaryExpression.Operator.Type, left, right) switch
        {
            (TokenType.Plus, int l, int r) => l + r,
            (TokenType.Minus, int l, int r) => l - r,
            (TokenType.Star, int l, int r) => l * r,
            (TokenType.Slash, int l, int r) => l / r,
            (TokenType.Percent, int l, int r) => l % r,
            (TokenType.Ampersand, int l, int r) => l & r,
            (TokenType.Caret, int l, int r) => l ^ r,
            (TokenType.Pipe, int l, int r) => l | r,
            (TokenType.Less, int l, int r) => l < r,
            (TokenType.LessLess, int l, int r) => l << r,
            (TokenType.LessEqual, int l, int r) => l <= r,
            (TokenType.Greater, int l, int r) => l > r,
            (TokenType.GreaterGreater, int l, int r) => l >> r,
            (TokenType.GreaterEqual, int l, int r) => l >= r,
            (TokenType.EqualEqual, int l, int r) => l == r,
            (TokenType.EqualEqual, bool l, bool r) => l == r,
            (TokenType.BangEqual, int l, int r) => l != r,
            (TokenType.BangEqual, bool l, bool r) => l != r,
            _ => throw new ArgumentOutOfRangeException(nameof(binaryExpression.Operator))
        };
    }

    private object? Evaluate(UnaryExpression unaryExpression, Frame frame)
    {
        var value = Evaluate(unaryExpression.Value, frame) ?? throw new InvalidOperationException("Cannot apply unary operation to null");

        return (unaryExpression.Operator.Type, value) switch
        {
            (TokenType.Plus, int i) => i,
            (TokenType.Minus, int i) => -i,
            (TokenType.Bang, bool b) => !b,
            (TokenType.Tilde, int i) => ~i,
            _ => throw new ArgumentOutOfRangeException(nameof(unaryExpression.Operator))
        };
    }

    private object? Evaluate(MatchExpression matchExpression, Frame frame)
    {
        var input = Evaluate(matchExpression.Input, frame);

        foreach (var matchCase in matchExpression.Cases)
        {
            switch (matchCase.Pattern)
            {
                case BindingMatchPattern bindingMatchPattern:
                    var bindingMatchFrame = frame.CreateChild();
                    bindingMatchFrame[bindingMatchPattern.Identifier.Text] = input;
            
                    return Evaluate(matchCase.ReturnValue, bindingMatchFrame);
                case ConstantMatchPattern constantMatchPattern:
                    switch (constantMatchPattern.Value.Literal, input)
                    {
                        case (int intMatch, int intInput) when intInput == intMatch:
                        case (bool boolMatch, bool boolInput) when boolInput == boolMatch:
                            return Evaluate(matchCase.ReturnValue, frame);
                    }
                    break;
                case NegationMatchPattern constantMatchPattern:
                    switch (constantMatchPattern.Value.Literal, input)
                    {
                        case (int intMatch, int intInput) when intInput != intMatch:
                        case (bool boolMatch, bool boolInput) when boolInput != boolMatch:
                            return Evaluate(matchCase.ReturnValue, frame);
                    }
                    break;
                case ComparisonMatchPattern comparisonMatchPattern:
                    switch (comparisonMatchPattern.Operator.Type, comparisonMatchPattern.CompareValue.Literal, input)
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
                case DiscardPattern:
                    return Evaluate(matchCase.ReturnValue, frame);
                default:
                    throw new ArgumentOutOfRangeException();
            }
        }

        throw new InvalidOperationException("No matching pattern found");
    }

    private class UserDefinedFunction(FunctionDeclarationStatement declaration, Frame closure) : ICallable
    {
        public Type[] Signature { get; } = [..declaration.Parameters.Select(parameter => parameter.IdentifierType.RuntimeType)];
        
        public object? Invoke(Interpreter interpreter, object?[] args)
        {
            var frame = closure.CreateChild();
            
            foreach (var (argument, parameter) in args.Zip(declaration.Parameters))
                frame[parameter.Identifier.Text] = argument;
            
            return interpreter.Evaluate(declaration.Body, frame);
        }
    }

    private class BuiltinFunction(Func<object?[], object?> invoke, Type[] parameters) : ICallable
    {
        public Type[] Signature => parameters;
        
        public object? Invoke(Interpreter interpreter, object?[] args) =>
            invoke(args);
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

internal interface ICallable
{
    Type[] Signature { get; }

    object? Invoke(Interpreter interpreter, object?[] args);
}
