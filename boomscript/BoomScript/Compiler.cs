using System.Reflection;
using System.Reflection.Emit;

namespace BoomScript;

public sealed class Compiler
{
    private readonly BoundProgram _program;
    private readonly TypeBuilder _typeBuilder;
    private readonly MethodBuilder _methodBuilder;
    private readonly ILGenerator _methodIL;
    private readonly Frame _frame;

    private Compiler(BoundProgram program)
    {
        _program = program;

        _typeBuilder = AssemblyBuilder.DefineDynamicAssembly(new AssemblyName { Name = "TempAssembly" }, AssemblyBuilderAccess.Run)
            .DefineDynamicModule("Module")
            .DefineType("Program",TypeAttributes.Public);
        
        _methodBuilder = _typeBuilder.DefineMethod("Main", MethodAttributes.Public | MethodAttributes.Static, CallingConventions.Standard, typeof(int), []);
        _methodIL = _methodBuilder.GetILGenerator();
        
        _frame = Frame.Default();
    }

    public static object? Run(BoundProgram program)
    {
        var type = new Compiler(program).Compile();
        var method = type.GetMethod("Main")!;
        return method.Invoke(null, null);
    }

    private Type Compile()
    {
        foreach (var expression in _program.Expressions)
            Emit(expression);
        
        _methodIL.Emit(OpCodes.Ret);
        return _typeBuilder.CreateType();
    }

    private void Emit(BoundExpression expression)
    {
        switch (expression)
        {
            case BoundAssignmentExpression assignmentExpression:
                if (!_frame.Locals.ContainsKey(assignmentExpression.Identifier))
                {
                    var type = assignmentExpression.Value.Type switch
                    {
                        BoundType.Unit => typeof(void),
                        BoundType.Int => typeof(int),
                        BoundType.Bool => typeof(bool),
                        _ => throw new ArgumentOutOfRangeException()
                    };
                    
                    var localBuilder = _methodIL.DeclareLocal(type);
                    _frame.Locals[assignmentExpression.Identifier] = localBuilder.LocalIndex;
                }
                
                Emit(assignmentExpression.Value);
                
                var indexToStore = _frame[assignmentExpression.Identifier];
                _methodIL.Emit(OpCodes.Stloc, indexToStore);
                break;
            case BoundBinaryExpression binaryExpression:
                Emit(binaryExpression.Left);
                Emit(binaryExpression.Right);
                switch (binaryExpression.Operator)
                {
                    case { Kind: BoundBinaryOperatorKind.Add, LeftType: BoundType.Int, RightType: BoundType.Int }:
                        _methodIL.Emit(OpCodes.Add);
                        break;
                    case { Kind: BoundBinaryOperatorKind.Mul, LeftType: BoundType.Int, RightType: BoundType.Int }:
                        _methodIL.Emit(OpCodes.Mul);
                        break;
                    case { Kind: BoundBinaryOperatorKind.Greater, LeftType: BoundType.Int, RightType: BoundType.Int }:
                        _methodIL.Emit(OpCodes.Cgt);
                        break;
                    case { Kind: BoundBinaryOperatorKind.Less, LeftType: BoundType.Int, RightType: BoundType.Int }:
                        _methodIL.Emit(OpCodes.Clt);
                        break;
                    default:
                        throw new ArgumentOutOfRangeException();
                }
                break;
            case BoundLiteralExpression literalExpression:
                switch (literalExpression.Value)
                {
                    case int i:
                        _methodIL.Emit(OpCodes.Ldc_I4, i);
                        break;
                    case bool b:
                        _methodIL.Emit(b ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0);
                        break;
                    default:
                        throw new InvalidOperationException("Unknown literal");
                }
                break;
            case BoundVariableExpression nameExpression:
                var indexToRead = _frame[nameExpression.Identifier];
                _methodIL.Emit(OpCodes.Ldloc, indexToRead);
                break;
            default:
                throw new ArgumentOutOfRangeException(nameof(expression));
        }
    }
    
    private record Frame(Dictionary<string, int> Locals)
    {
        public static Frame Default() => new(new Dictionary<string, int>());

        public int this[string index]
        {
            get => Locals[index];
            set => Locals[index] = value;
        }
    }
}