namespace Gleamy;

internal class Interpreter(SyntaxTree tree)
{
    private Frame _frame = new();
    
    public static object? Evaluate(string source)
    {
        var tree = Parser.Parse(source);
        return new Interpreter(tree).Evaluate();
    }

    private object? Evaluate()
    {
        object? result = null;   
        
        foreach (var statement in tree.Statements)
            result = Evaluate(statement);

        return result;
    }

    private object? Evaluate(Statement statement)
    {
        switch (statement)
        {
            case BindingDeclarationStatement bindingDeclarationStatement:
                return Evaluate(bindingDeclarationStatement);
            case BlockStatement blockStatement:
                return Evaluate(blockStatement);
            case ExpressionStatement expressionStatement:
                return Evaluate(expressionStatement);
            case FunctionDeclarationStatement functionDeclarationStatement:
                return Evaluate(functionDeclarationStatement);
            default:
                throw new ArgumentOutOfRangeException(nameof(statement));
        }
    }

    private object? Evaluate(BindingDeclarationStatement bindingDeclarationStatement)
    {
        var value = Evaluate(bindingDeclarationStatement.Value);
        _frame.Set(bindingDeclarationStatement.Identifier.Text, value);
        return null;
    }

    private object? Evaluate(FunctionDeclarationStatement functionDeclarationStatement)
    {
        var userDefinedFunction = new UserDefinedFunction(functionDeclarationStatement);
        _frame.Set(functionDeclarationStatement.Identifier.Text, userDefinedFunction);
        return null;
    }
    
    private object? Evaluate(BlockStatement blockStatement)
    {
        object? result = null;
        
        foreach (var statement in blockStatement.Statements)
            result = Evaluate(statement);

        return result;
    }
    
    private object? Evaluate(ExpressionStatement expressionStatement)
    {
        return Evaluate(expressionStatement.Expression);
    }

    private object? Evaluate(Expression expression)
    {
        switch (expression)
        {
            case UnaryExpression unaryExpression:
                return Evaluate(unaryExpression);
            case BinaryExpression binaryExpression:
                return Evaluate(binaryExpression);
            case CallExpression callExpression:
                return Evaluate(callExpression);
            case LiteralExpression literalExpression:
                return Evaluate(literalExpression);
            case NameExpression nameExpression:
                return Evaluate(nameExpression);
            case MatchExpression matchExpression:
                return Evaluate(matchExpression);
            case ParenthesizedExpression parenthesizedExpression:
                return Evaluate(parenthesizedExpression);
            case LogicalAndExpression andExpression:
                return Evaluate(andExpression);
            case LogicalOrExpression orExpression:
                return Evaluate(orExpression);
            default:
                throw new ArgumentOutOfRangeException(nameof(expression));
        }
    }
    
    private object? Evaluate(CallExpression callExpression)
    {
        var binding = _frame.Get(callExpression.Identifier.Text);
        if (binding is not ICallable callable)
            throw new InvalidOperationException("Not callable");

        if (callable.Parameters.Length != callExpression.Arguments.Length)
            throw new ArgumentException("Invalid number of arguments");

        _frame = new Frame(_frame);

        try
        {
            foreach (var (argument, parameter) in callExpression.Arguments.Zip(callable.Parameters))
            {
                var argumentValue = Evaluate(argument);
                if (parameter.IdentifierType.RuntimeType != argumentValue?.GetType())
                    throw new InvalidOperationException("Cannot apply parameter int");
            
                _frame.Set(parameter.Identifier.Text, argumentValue);
            }
        
            return callable.Invoke(this);
        }
        finally
        {
            _frame = _frame.Parent!;
        }
    }

    private object? Evaluate(NameExpression nameExpression)
    {
        return _frame.Get(nameExpression.Identifier.Text);
    }

    private object Evaluate(LiteralExpression literalExpression)
    {
        return literalExpression.Value.Literal!;
    }

    private object? Evaluate(ParenthesizedExpression parenthesizedExpression)
    {
        return Evaluate(parenthesizedExpression.Expression);
    }

    private object? Evaluate(LogicalAndExpression logicalAndExpression)
    {
        var left =  Evaluate(logicalAndExpression.Left) ?? throw new InvalidOperationException("Cannot apply && to null");
        if (left is not bool leftBool)
            throw new InvalidOperationException("Cannot apply && to non-boolean");

        if (!leftBool)
            return false;
        
        var right = Evaluate(logicalAndExpression.Right) ?? throw new InvalidOperationException("Cannot apply && to null");;
        if (right is not bool rightBool)
            throw new InvalidOperationException("Cannot apply && to non-boolean");

        return rightBool;
    }

    private object? Evaluate(LogicalOrExpression logicalOrExpression)
    {
        var left =  Evaluate(logicalOrExpression.Left) ?? throw new InvalidOperationException("Cannot apply && to null");
        if (left is not bool leftBool)
            throw new InvalidOperationException("Cannot apply && to non-boolean");

        if (leftBool)
            return true;
        
        var right = Evaluate(logicalOrExpression.Right) ?? throw new InvalidOperationException("Cannot apply && to null");;
        if (right is not bool rightBool)
            throw new InvalidOperationException("Cannot apply && to non-boolean");

        return rightBool;
    }

    private object? Evaluate(BinaryExpression binaryExpression)
    {
        var left =  Evaluate(binaryExpression.Left) ?? throw new InvalidOperationException("Cannot apply binary operation to null");
        var right = Evaluate(binaryExpression.Right) ?? throw new InvalidOperationException("Cannot apply binary operation to null");;

        return (binaryExpression.Operator.Type, left, right) switch
        {
            (TokenType.Plus, int l, int r) => l + r,
            (TokenType.Minus, int l, int r) => l - r,
            (TokenType.Star, int l, int r) => l * r,
            (TokenType.Slash, int l, int r) => l / r,
            (TokenType.Percent, int l, int r) => l % r,
            (TokenType.Ampersand, int l, int r) => l & r,
            (TokenType.Pipe, int l, int r) => l | r,
            (TokenType.Less, int l, int r) => l < r,
            (TokenType.LessEqual, int l, int r) => l <= r,
            (TokenType.Greater, int l, int r) => l > r,
            (TokenType.GreaterEqual, int l, int r) => l >= r,
            (TokenType.EqualEqual, int l, int r) => l == r,
            (TokenType.EqualEqual, bool l, bool r) => l == r,
            (TokenType.BangEqual, int l, int r) => l != r,
            (TokenType.BangEqual, bool l, bool r) => l != r,
            _ => throw new ArgumentOutOfRangeException(nameof(binaryExpression.Operator))
        };
    }

    private object? Evaluate(UnaryExpression unaryExpression)
    {
        var value =  Evaluate(unaryExpression.Value) ?? throw new InvalidOperationException("Cannot apply unary operation to null");

        return (unaryExpression.Operator.Type, value) switch
        {
            (TokenType.Plus, int i) => i,
            (TokenType.Minus, int i) => -i,
            (TokenType.Bang, bool b) => !b,
            _ => throw new ArgumentOutOfRangeException(nameof(unaryExpression.Operator))
        };
    }

    private object? Evaluate(MatchExpression matchExpression)
    {
        var input = Evaluate(matchExpression.Input);

        foreach (var matchCase in matchExpression.Cases)
        {
            switch (matchCase.Pattern)
            {
                case BindingMatchPattern bindingMatchPattern:
                    var oldEnvironment = _frame;
                    try
                    {
                        _frame = new Frame(_frame);
                        _frame.Set(bindingMatchPattern.Identifier.Text, input);
            
                        return Evaluate(matchCase.ReturnValue);
                    }
                    finally
                    {
                        _frame = oldEnvironment;    
                    }
                case ConstantMatchPattern constantMatchPattern:
                    switch (constantMatchPattern.Value.Literal, input)
                    {
                        case (int intMatch, int intInput) when intInput == intMatch:
                        case (bool boolMatch, bool boolInput) when boolInput == boolMatch:
                            return Evaluate(matchCase.ReturnValue);
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
                            return Evaluate(matchCase.ReturnValue);
                    }
                    break;
                case DiscardPattern:
                    return Evaluate(matchCase.ReturnValue);
                default:
                    throw new ArgumentOutOfRangeException();
            }
        }

        return null;
    }

    private class UserDefinedFunction(FunctionDeclarationStatement declaration) : ICallable
    {
        public Parameter[] Parameters => declaration.Parameters;
        
        public object? Invoke(Interpreter interpreter) =>
            interpreter.Evaluate(declaration.Body);
    }
}

internal class Frame(Frame? parent = null)
{
    private readonly Dictionary<string, object?> _locals = new();

    public Frame? Parent => parent;
        
    public object? Get(string key)
    {
        if (_locals.TryGetValue(key, out var result))
            return result;
            
        return parent?.Get(key);
    }

    public void Set(string key, object? value)
    {
        if (!_locals.TryAdd(key, value))
            throw new InvalidOperationException("Cannot redeclare local");
    }
}

internal interface ICallable
{
    Parameter[] Parameters { get; }

    object? Invoke(Interpreter interpreter);
}
