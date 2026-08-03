namespace Arya.Tests;

public class ArrayTests
{
    public class Equality
    {
        [Fact]
        public void EqualWhenInstanceIsSame()
        {
            var array = new Array([1, 2, 3], [5, 10]);

            Assert.Equal(array, array);
        }
        
        [Fact]
        public void EqualWhenElementsAndDimensionsAreEqual()
        {
            var array1 = new Array([1, 2, 3], [5, 10]);
            var array2 = new Array([1, 2, 3], [5, 10]);

            Assert.Equal(array1, array2);
        }
        
        [Fact]
        public void UnequalWhenElementsAreDifferent()
        {
            var array1 = new Array([1, 2, 3], [5, 10]);
            var array2 = new Array([4, 5, 6], [5, 10]);

            Assert.NotEqual(array1, array2);
        }
        
        [Fact]
        public void UnequalWhenFewerElements()
        {
            var array1 = new Array([1, 2, 3], [5, 10]);
            var array2 = new Array([1, 2], [5, 10]);

            Assert.NotEqual(array1, array2);
        }
        
        [Fact]
        public void UnequalWhenMoreElements()
        {
            var array1 = new Array([1, 2, 3], [5, 10]);
            var array2 = new Array([1, 2, 3, 4], [5, 10]);

            Assert.NotEqual(array1, array2);
        }
        
        [Fact]
        public void UnequalWhenElementOrderIsDifferent()
        {
            var array1 = new Array([1, 2, 3], [5, 10]);
            var array2 = new Array([3, 2, 1], [5, 10]);

            Assert.NotEqual(array1, array2);
        }
        
        [Fact]
        public void UnequalWhenDimensionOrderIsDifferent()
        {
            var array1 = new Array([1, 2, 3], [5, 10]);
            var array2 = new Array([1, 2, 3], [10, 5]);

            Assert.NotEqual(array1, array2);
        }
        
        [Fact]
        public void UnequalWhenDimensionsAreDifferent()
        {
            var array1 = new Array([1, 2, 3], [5, 10]);
            var array2 = new Array([1, 2, 3], [7, 21]);

            Assert.NotEqual(array1, array2);
        }
        
        [Fact]
        public void UnequalWhenFewerDimensions()
        {
            var array1 = new Array([1, 2, 3], [5, 10]);
            var array2 = new Array([1, 2, 3], [5]);

            Assert.NotEqual(array1, array2);
        }
        
        [Fact]
        public void UnequalWhenMoreDimensions()
        {
            var array1 = new Array([1, 2, 3], [5, 10]);
            var array2 = new Array([1, 2, 3], [5, 10, 20]);

            Assert.NotEqual(array1, array2);
        }
    }
    
    public class HashCode
    {
        [Fact]
        public void SameHashCodeForSameInstance()
        {
            var array = new Array([1, 2, 3], [5, 10]);

            Assert.Equal(array.GetHashCode(), array.GetHashCode());
        }
        
        [Fact]
        public void SameHashCodeWhenElementsAndDimensionsAreEqual()
        {
            var array1 = new Array([1, 2, 3], [5, 10]);
            var array2 = new Array([1, 2, 3], [5, 10]);

            Assert.Equal(array1.GetHashCode(), array2.GetHashCode());
        }
        
        [Fact]
        public void DifferentHashCodeWhenElementsAreDifferent()
        {
            var array1 = new Array([1, 2, 3], [5, 10]);
            var array2 = new Array([4, 5, 6], [5, 10]);

            Assert.NotEqual(array1.GetHashCode(), array2.GetHashCode());
        }
        
        [Fact]
        public void DifferentHashCodeWhenFewerElements()
        {
            var array1 = new Array([1, 2, 3], [5, 10]);
            var array2 = new Array([1, 2], [5, 10]);

            Assert.NotEqual(array1.GetHashCode(), array2.GetHashCode());
        }
        
        [Fact]
        public void DifferentHashCodeWhenMoreElements()
        {
            var array1 = new Array([1, 2, 3], [5, 10]);
            var array2 = new Array([1, 2, 3, 4], [5, 10]);

            Assert.NotEqual(array1.GetHashCode(), array2.GetHashCode());
        }
        
        [Fact]
        public void DifferentHashCodeWhenElementOrderIsDifferent()
        {
            var array1 = new Array([1, 2, 3], [5, 10]);
            var array2 = new Array([3, 2, 1], [5, 10]);

            Assert.NotEqual(array1.GetHashCode(), array2.GetHashCode());
        }
        
        [Fact]
        public void DifferentHashCodeWhenDimensionOrderIsDifferent()
        {
            var array1 = new Array([1, 2, 3], [5, 10]);
            var array2 = new Array([1, 2, 3], [10, 5]);

            Assert.NotEqual(array1.GetHashCode(), array2.GetHashCode());
        }
        
        [Fact]
        public void DifferentHashCodeWhenDimensionsAreDifferent()
        {
            var array1 = new Array([1, 2, 3], [5, 10]);
            var array2 = new Array([1, 2, 3], [7, 21]);

            Assert.NotEqual(array1.GetHashCode(), array2.GetHashCode());
        }
        
        [Fact]
        public void DifferentHashCodeWhenFewerDimensions()
        {
            var array1 = new Array([1, 2, 3], [5, 10]);
            var array2 = new Array([1, 2, 3], [5]);

            Assert.NotEqual(array1.GetHashCode(), array2.GetHashCode());
        }
        
        [Fact]
        public void DifferentHashCodeWhenMoreDimensions()
        {
            var array1 = new Array([1, 2, 3], [5, 10]);
            var array2 = new Array([1, 2, 3], [5, 10, 20]);

            Assert.NotEqual(array1.GetHashCode(), array2.GetHashCode());
        }
    }
}