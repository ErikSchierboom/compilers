namespace BoomScript;

using System;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;

internal static class IlDisassembler
{
    private static readonly Dictionary<ushort, OpCode> _opcodes;

    static IlDisassembler()
    {
        _opcodes = new Dictionary<ushort, OpCode>();

        foreach (var field in typeof(OpCodes).GetFields(
                     BindingFlags.Public | BindingFlags.Static))
        {
            if (field.GetValue(null) is OpCode opcode)
            {
                _opcodes[(ushort)opcode.Value] = opcode;
            }
        }
    }

    public static void Disassemble(MethodInfo method)
    {
        var body = method.GetMethodBody();
        if (body == null)
        {
            Console.WriteLine("No method body.");
            return;
        }

        var il = body.GetILAsByteArray();
        int i = 0;

        while (i < il.Length)
        {
            int offset = i;

            // Read opcode (1 or 2 bytes)
            ushort value = il[i++];
            if (value == 0xFE)
            {
                value = (ushort)(0xFE00 | il[i++]);
            }

            if (!_opcodes.TryGetValue(value, out var opcode))
            {
                Console.WriteLine($"IL_{offset:X4}: <unknown opcode>");
                break;
            }

            object operand = null;

            switch (opcode.OperandType)
            {
                case OperandType.InlineNone:
                    break;

                case OperandType.ShortInlineI:
                    operand = (sbyte)il[i++];
                    break;

                case OperandType.InlineI:
                    operand = BitConverter.ToInt32(il, i);
                    i += 4;
                    break;

                case OperandType.InlineI8:
                    operand = BitConverter.ToInt64(il, i);
                    i += 8;
                    break;

                case OperandType.ShortInlineR:
                    operand = BitConverter.ToSingle(il, i);
                    i += 4;
                    break;

                case OperandType.InlineR:
                    operand = BitConverter.ToDouble(il, i);
                    i += 8;
                    break;

                case OperandType.ShortInlineBrTarget:
                    operand = (sbyte)il[i++] + i;
                    break;

                case OperandType.InlineBrTarget:
                    operand = BitConverter.ToInt32(il, i) + i + 4;
                    i += 4;
                    break;

                case OperandType.ShortInlineVar:
                    operand = il[i++];
                    break;

                case OperandType.InlineVar:
                    operand = BitConverter.ToUInt16(il, i);
                    i += 2;
                    break;

                case OperandType.InlineString:
                case OperandType.InlineField:
                case OperandType.InlineMethod:
                case OperandType.InlineType:
                case OperandType.InlineTok:
                case OperandType.InlineSig:
                    operand = BitConverter.ToInt32(il, i);
                    i += 4;
                    break;

                default:
                    throw new NotSupportedException(
                        $"Unsupported operand type: {opcode.OperandType}");
            }

            Console.WriteLine(
                operand == null
                    ? $"IL_{offset:X4}: {opcode.Name}"
                    : $"IL_{offset:X4}: {opcode.Name} {operand}"
            );
        }
    }
}
