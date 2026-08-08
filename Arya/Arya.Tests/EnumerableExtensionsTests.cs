namespace Arya.Tests;

public static class EnumerableExtensionsTests
{
    public class Repeat
    {
        [Fact]
        public void EmptySequenceIsNotRepeated()
        {
            int[] empty = [];
            var cycled = empty.Repeat();
            Assert.Empty(cycled);
        }

        [Fact]
        public void TakeFewerElements()
        {
            int[] values = [1, 2, 3];
            var result = values.Repeat().Take(values.Length - 1);
            Assert.Equal([1, 2], result);
        }

        [Fact]
        public void TakeSameNumberOfElements()
        {
            int[] values = [1, 2, 3];
            var result = values.Repeat().Take(values.Length);
            Assert.Equal(values, result);
        }

        [Fact]
        public void TakeMoreElements()
        {
            int[] values = [1, 3, 5];
            var result = values.Repeat().Take(values.Length + 2);
            Assert.Equal([1, 3, 5, 1, 3], result);
        }
    }
}
