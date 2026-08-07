namespace Arya.Tests;

public static class EnumerableExtensionsTests
{
    public class Cycle
    {
        [Fact]
        public void ReturnsEmptySequenceForEmptySequence()
        {
            int[] empty = [];
            var cycled = empty.Cycle(5);
            Assert.Empty(cycled);
        }

        [Fact]
        public void ReturnsEmptySequenceForCycleLengthOfZero()
        {
            int[] values = [1, 2, 3];
            var cycled = values.Cycle(0);
            Assert.Empty(cycled);
        }

        [Fact]
        public void ReturnsFullSequenceWhenCycleLengthIsEqualToSequenceLength()
        {
            int[] values = [1, 2, 3];
            var result = values.Cycle(values.Length);
            Assert.Equal(values, result);
        }

        [Fact]
        public void ReturnsSubSequenceWhenCycleLengthIsLessThanSequenceLength()
        {
            int[] values = [1, 2, 3];
            var result = values.Cycle(2);
            Assert.Equal([1, 2], result);
        }

        [Fact]
        public void RepeatsSequenceWhenCycleLengthExceedsSequenceLength()
        {
            int[] values = [1, 2, 3];
            var result = values.Cycle(5).ToArray();
            Assert.Equal([1, 2, 3, 1, 2], result);
        }
    }
}
