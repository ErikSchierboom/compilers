using System.Reflection;
using System.Reflection.Emit;

namespace BoomScript;

public sealed class Compiler
{
    public static System.Type Run(BoundProgram program)
    {
        AssemblyName myAssemblyName = new AssemblyName
        {
            Name = "TempAssembly"
        };

        var myAssemblyBuilder = AssemblyBuilder.DefineDynamicAssembly(myAssemblyName, AssemblyBuilderAccess.Run);

        // Define a dynamic module in this assembly.
        ModuleBuilder myModuleBuilder = myAssemblyBuilder.
            DefineDynamicModule("TempModule");

        // Define a runtime class with specified name and attributes.
        TypeBuilder myTypeBuilder = myModuleBuilder.DefineType
            ("TempClass",TypeAttributes.Public);

        // Add 'Greeting' field to the class, with the specified attribute and type.
        FieldBuilder greetingField = myTypeBuilder.DefineField("Greeting",
            typeof(String), FieldAttributes.Public);
        System.Type[] myMethodArgs = { typeof(String) };

        // Add 'MyMethod' method to the class, with the specified attribute and signature.
        MethodBuilder myMethod = myTypeBuilder.DefineMethod("MyMethod",
            MethodAttributes.Public, CallingConventions.Standard, null,myMethodArgs);

        ILGenerator methodIL = myMethod.GetILGenerator();
        methodIL.EmitWriteLine("In the method...");
        methodIL.Emit(OpCodes.Ldarg_0);
        methodIL.Emit(OpCodes.Ldarg_1);
        methodIL.Emit(OpCodes.Stfld, greetingField);
        methodIL.Emit(OpCodes.Ret);
        return myTypeBuilder.CreateType();
    }
}