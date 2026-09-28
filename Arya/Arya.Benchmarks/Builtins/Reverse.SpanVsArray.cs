using BenchmarkDotNet.Attributes;

namespace Arya.Benchmarks.Builtins;

[MemoryDiagnoser]
public class ReverseSpanVsArray
{
    private readonly Array<int> _array;

    [Params(0, 1, 10, 100)]
    public int N { get; set; }

    public ReverseSpanVsArray()
    {
        var data = new byte[N];
        new Random(42).NextBytes(data);

        _array = new Array<int>(new Shape(N), [..data]);
    }

    [Benchmark]
    public Array<int> ArrayCopy()
    {
        if (_array.Elements.Length < 2)
            return _array;

        var sortedElements = new int[_array.Elements.Length];

        var rowSize = _array.Elements.Length / _array.Shape.RowCount;
        for (var row = 0; row < _array.Shape.RowCount; row++)
        {
            var sourceIndex = _array.Elements.Length - (row + 1) * rowSize;
            var destinationIndex = row * rowSize;
            Array.Copy(_array.Elements, sourceIndex, sortedElements, destinationIndex, rowSize);
        }

        return _array with { Elements = sortedElements };
    }

    [Benchmark]
    public Array<int> SpanCopy()
    {
        if (_array.Elements.Length < 2)
            return _array;

        var sortedElements = new int[_array.Elements.Length];

        var src = _array.Elements.AsSpan();
        var dst = sortedElements.AsSpan();
        var rowCount = _array.Shape.RowCount;
        var rowSize = src.Length / rowCount;

        for (var row = 0; row < rowCount; row++)
        {
            src.Slice(src.Length - (row + 1) * rowSize, rowSize)
                .CopyTo(dst.Slice(row * rowSize, rowSize));
        }

        return _array with { Elements = sortedElements };
    }
}
