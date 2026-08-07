using System.Diagnostics.CodeAnalysis;
using System.Text.Json;
using Arya;
using Arya.Tests;
using Xunit.Sdk;

[assembly: RegisterXunitSerializer(typeof(ValueSerializer), typeof(Value))]

namespace Arya.Tests;

public class ValueSerializer : IXunitSerializer
{
    public bool IsSerializable(Type type, object? value, [NotNullWhen(false)] out string? failureReason)
    {
        if (type != typeof(Value))
        {
            failureReason = $"Can only serialize {nameof(Value)} instances";
            return false;
        }

        failureReason = null;
        return true;
    }
    
    public object Deserialize(Type type, string serializedValue) => JsonSerializer.Deserialize<Value>(serializedValue)!;

    public string Serialize(object value) => JsonSerializer.Serialize(value);
}
