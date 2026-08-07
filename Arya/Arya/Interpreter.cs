namespace Arya;

public sealed record Scope
{
    private readonly Scope? _parent;
    private Dictionary<string, Value?> Values => field ??= new Dictionary<string, Value?>();
    
    public Scope(Scope? parent = null) => _parent = parent;

    public Scope CreateChild() => new(this);

    public Value? this[string key]
    {
        get
        {
            if (Values.TryGetValue(key, out var result))
                return result;
            
            return _parent?[key];
        }
        set
        {
            if (!Values.TryAdd(key, value))
                throw new InvalidOperationException("Cannot redeclare local");;
        }
    }
}

public class Interpreter
{
    private readonly List<Expression> _expressions;

    private Interpreter(List<Expression> expressions) => _expressions = expressions;

    public static Value? Evaluate(string code)
    {
        var expressions = Parser.Parse(code);
        return new Interpreter(expressions).Evaluate();
    }

    private Value? Evaluate()
    {
        Value? result = null;
        
        var defaultScope = CreateDefaultScope();

        foreach (var expression in _expressions)
            result = Evaluate(expression, defaultScope);

        return result;
    }

    private Value Evaluate(Expression expression, Scope scope)
    {
        switch (expression)
        {
            case ArrayLiteralExpression arrayLiteral:
                return Evaluate(arrayLiteral, scope);
            // case UnaryExpression unary:
            //     var operand = Evaluate(unary.Operand, scope);
            //     switch (unary.Operator.Type, operand)
            //     {
            //         case (TokenType.Plus, Integer):
            //         case (TokenType.Plus, Array):
            //             return operand;
            //         case (TokenType.Minus, Integer i):
            //             return new Integer(-i.Value);
            //         case (TokenType.Minus, Array a):
            //             return Array.UnaryOp(a, i => -i);
            //         default:
            //             throw new InvalidOperationException("Invalid unary expression");
            //     }
            case BinaryExpression binary:
                var left = Evaluate(binary.Left, scope);
                var right = Evaluate(binary.Right, scope);

                switch (binary.Operator.Type, left, right)
                {
                    case (TokenType.Plus, Integers l, Integers r):
                        throw new NotImplementedException();
                    default:
                        throw new InvalidOperationException("Invalid binary expression");
                }
            // switch (binary.Operator.Type, left, right)
                // {
                //     case (TokenType.Plus, Integer l, Integer r): 
                //         return new Integer(l.Value + r.Value);
                //     case (TokenType.Plus, Integer l, Array r): 
                //         return Array.BinaryOp(l, r, (li, ri) => li + ri);
                //     case (TokenType.Plus, Array l, Integer r): 
                //         return Array.BinaryOp(l, r, (li, ri) => li + ri);
                //     case (TokenType.Plus, Array l, Array r): 
                //         return Array.BinaryOp(l, r, (li, ri) => li + ri);
                //     case (TokenType.Plus, Integer l, String r): 
                //         return r.RotateChars(l.Value);
                //     case (TokenType.Plus, String l, Integer r): 
                //         return l.RotateChars(r.Value);
                //     
                //     case (TokenType.Minus, Integer l, Integer r): 
                //         return new Integer(l.Value - r.Value);
                //     case (TokenType.Minus, Integer l, Array r): 
                //         return Array.BinaryOp(l, r, (li, ri) => li - ri);
                //     case (TokenType.Minus, Array l, Integer r): 
                //         return Array.BinaryOp(l, r, (li, ri) => li - ri);
                //     case (TokenType.Minus, Array l, Array r): 
                //         return Array.BinaryOp(l, r, (li, ri) => li - ri);
                //     case (TokenType.Minus, Integer l, String r): 
                //         return r.RotateChars(-l.Value);
                //     case (TokenType.Minus, String l, Integer r): 
                //         return l.RotateChars(-r.Value);
                //     
                //     case (TokenType.Star, Integer l, Integer r): 
                //         return new Integer(l.Value * r.Value);
                //     
                //     case (TokenType.PlusPlus, Integer l, Integer r): 
                //         return new Integer(l.Value + r.Value);
                //     case (TokenType.PlusPlus, Array l, Array r): 
                //         return Array.Append(l, r);
                //     case (TokenType.PlusPlus, String l, String r): 
                //         return new String(l.Value + r.Value);
                //     case (TokenType.PlusPlus, Value l, Array r): 
                //         return Array.Append(l, r);
                //     case (TokenType.PlusPlus, Array l, Value r): 
                //         return Array.Append(l, r);
                //     default:
                //         throw new ArgumentOutOfRangeException(nameof(binary.Operator.Type));
                // }
            case LiteralExpression literal:
                switch (literal.Value.Type)
                {
                    case TokenType.String:
                        // TODO: create constructor overload
                        var chars = ((string)literal.Value.Literal!).ToCharArray();
                        return new Chars(new Shape(chars.Length), chars);
                    case TokenType.Number:
                        // TODO: create constructor overload
                        return new Integers(Shape.Scalar, (int)literal.Value.Literal!);
                    default:
                        throw new ArgumentOutOfRangeException(nameof(literal.Value.Type));
                }
            case CallExpression call:
                if (scope[call.FunctionName.Text] is not Function function)
                    throw new InvalidOperationException("Can only call functions");
                
                if (call.Arguments.Length != function.Arity)
                    throw new InvalidOperationException("Invalid number of arguments");

                var arguments = call.Arguments.Select(arg => Evaluate(arg, scope)).ToArray();
                return function.Invoke(arguments);
            case ParenthesizedExpression parenthesized:
                return Evaluate(parenthesized.Expression, scope);
            default:
                throw new ArgumentOutOfRangeException(nameof(expression));
        }
    }

    private Value Evaluate(ArrayLiteralExpression arrayLiteral, Scope scope)
    {
        if (arrayLiteral.Elements.Length == 0)
            return Empty.Instance;
                
        Type? elementType = null;
        Shape? shape = null;
                
        Value[] newElements = new Value[arrayLiteral.Elements.Length];

        for (var index = 0; index < arrayLiteral.Elements.Length; index++)
        {
            var evaluatedElement = Evaluate(arrayLiteral.Elements[index], scope);

            if (elementType is null)
                elementType = evaluatedElement.GetType();
            else if (elementType != evaluatedElement.GetType())
                throw new InvalidOperationException("Array elements must be of the same type.");

            if (shape is null)
                shape = evaluatedElement.Shape;
            else if (shape != evaluatedElement.Shape)
                throw new InvalidOperationException("Array elements must have the same shape.");

            newElements[index] = evaluatedElement;
        }

        var newShape = shape!.Prepend(newElements.Length);

        if (elementType == typeof(Integers))
            return new Integers(newShape, [..newElements.Cast<Integers>().SelectMany(array => array.Elements)]);
                
        if (elementType == typeof(Chars))
            return new Chars(newShape, [..newElements.Cast<Chars>().SelectMany(array => array.Elements)]);
                
        if (elementType == typeof(Empty))
            return Empty.Instance;
                
        throw new InvalidOperationException("Invalid array element type");
    }

    private static Scope CreateDefaultScope()
    {
        var scope = new Scope();
        
        // foreach (var builtinFunction in BuiltinFunctions.All)
        //     scope[builtinFunction.Name] = builtinFunction;

        return scope;
    }
}
