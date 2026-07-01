namespace Gleamy;

internal class Interpreter(SyntaxTree tree)
{
    public Environment Environment { get; set; } = new();
    
    public object? Evaluate()
    {
        object? result = null;   
        
        foreach (var statement in tree.Statements)
            result = Evaluate(statement);

        return result;
    }

    public object? Evaluate(Statement statement)
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
        Environment.Set(bindingDeclarationStatement.Identifier.Text, value);
        return null;
    }

    private object? Evaluate(FunctionDeclarationStatement functionDeclarationStatement)
    {
        var userDefinedFunction = new UserDefinedFunction(functionDeclarationStatement);
        Environment.Set(functionDeclarationStatement.Identifier.Text, userDefinedFunction);
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
            default:
                throw new ArgumentOutOfRangeException(nameof(expression));
        }
    }
    
    private object? Evaluate(CallExpression nameExpression)
    {
        var binding = Environment.Get(nameExpression.Identifier.Text);
        if (binding is not Callable callable)
            throw new InvalidOperationException("Not callable");

        var arguments = nameExpression.Arguments.Select(Evaluate).ToArray();
        return callable.Invoke(this, arguments);
    }

    private object? Evaluate(NameExpression nameExpression)
    {
        return Environment.Get(nameExpression.Identifier.Text);
    }

    private object? Evaluate(LiteralExpression literalExpression)
    {
        return literalExpression.Value.Type switch
        {
            TokenType.Number => int.Parse(literalExpression.Value.Text),
            _ => throw new ArgumentOutOfRangeException(nameof(literalExpression.Value))
        };
    }

    private object? Evaluate(BinaryExpression binaryExpression)
    {
        var left =  Evaluate(binaryExpression.Left) ?? throw new InvalidOperationException("Cannot apply binary operation to null");
        var right = Evaluate(binaryExpression.Right) ?? throw new InvalidOperationException("Cannot apply binary operation to null");;

        return binaryExpression.Operator.Type switch
        {
            TokenType.Plus  => (int)left + (int)right,
            TokenType.Minus => (int)left - (int)right,
            TokenType.Star  => (int)left * (int)right,
            TokenType.Slash => (int)left / (int)right,
            _ => throw new ArgumentOutOfRangeException(nameof(binaryExpression.Operator))
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
                    var oldEnvironment = Environment;
                    try
                    {
                        Environment = new Environment(Environment);
                        Environment.Set(bindingMatchPattern.Identifier.Text, input);
            
                        return Evaluate(matchCase.ReturnValue);
                    }
                    finally
                    {
                        Environment = oldEnvironment;    
                    }
                case ConstantMatchPattern constantMatchPattern:
                    switch (constantMatchPattern.Value.Type)
                    {
                        case TokenType.Number:
                            if (input is null)
                                return null;
                            
                            var matchValue = int.Parse(constantMatchPattern.Value.Text);
                            if (matchValue.Equals(input))
                                return Evaluate(matchCase.ReturnValue);

                            break;
                    }
                    break;
                case DiscardPattern _:
                    return Evaluate(matchCase.ReturnValue);
                default:
                    throw new ArgumentOutOfRangeException();
            }
        }

        return null;
    }
}

internal class Environment(Environment? parent = null)
{
    private readonly Dictionary<string, object?> _locals = new();

    public Environment? Parent => parent;
        
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

internal interface Callable
{
    object? Invoke(Interpreter interpreter, params object?[] args);
}

internal class UserDefinedFunction(FunctionDeclarationStatement declaration) : Callable
{
    public object? Invoke(Interpreter interpreter, params object?[] args)
    {
        if (args.Length != declaration.Parameters.Length)
            throw new ArgumentException("Invalid number of arguments");
        
        var oldEnvironment = interpreter.Environment;
        try
        {
            interpreter.Environment = new Environment(interpreter.Environment);
            
            foreach (var (parameter, arg) in declaration.Parameters.Zip(args))
                interpreter.Environment.Set(parameter.Identifier.Text, arg);
            
            return interpreter.Evaluate(declaration.Body);
        }
        finally
        {
            interpreter.Environment = oldEnvironment;    
        }
    }
}
