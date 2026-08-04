namespace Arya.Tests;

public class ValueTests
{
    public class Integers
    {
        public class Equality
        {
            [Fact]
            public void EqualWhenSameInstance()
            {
                var integer = new Integer(1);

                Assert.Equal(integer, integer);
            }

            [Fact]
            public void EqualWhenValueIsEqual()
            {
                var integer1 = new Integer(1);
                var integer2 = new Integer(1);

                Assert.Equal(integer1, integer2);
            }

            [Fact]
            public void UnequalWhenValueIsUnequal()
            {
                var integer1 = new Integer(1);
                var integer2 = new Integer(2);

                Assert.NotEqual(integer1, integer2);
            }
        }
        
        public class HashCode
        {
            [Fact]
            public void SameHashCodeWhenSameInstance()
            {
                var integer = new Integer(1);

                Assert.Equal(integer.GetHashCode(), integer.GetHashCode());
            }

            [Fact]
            public void SameHashCodeWhenValueIsEqual()
            {
                var integer1 = new Integer(1);
                var integer2 = new Integer(1);

                Assert.Equal(integer1.GetHashCode(), integer2.GetHashCode());
            }

            [Fact]
            public void DifferentHashCodeWhenValueIsUnequal()
            {
                var integer1 = new Integer(1);
                var integer2 = new Integer(2);

                Assert.NotEqual(integer1.GetHashCode(), integer2.GetHashCode());
            }
        }
    }

    public class Arrays
    {
        public class Equality
        {
            [Fact]
            public void EqualWhenSameInstance()
            {
                var array = new Array([new Integer(1), new Integer(2), new Integer(3)]);

                Assert.Equal(array, array);
            }
            
            [Fact]
            public void EqualWhenElementsAreEqualAndInSameOrder()
            {
                var array1 = new Array([new Integer(1), new Integer(2), new Integer(3)]);
                var array2 = new Array([new Integer(1), new Integer(2), new Integer(3)]);

                Assert.Equal(array1, array2);
            }
            
            [Fact]
            public void UnequalWhenSameNumberOfElementsButElementsAreUnequal()
            {
                var array1 = new Array([new Integer(1), new Integer(2), new Integer(3)]);
                var array2 = new Array([new Integer(4), new Integer(5), new Integer(6)]);

                Assert.NotEqual(array1, array2);
            }
            
            [Fact]
            public void UnequalWhenElementsAreEqualButInDifferentOrder()
            {
                var array1 = new Array([new Integer(1), new Integer(2), new Integer(3)]);
                var array2 = new Array([new Integer(3), new Integer(2), new Integer(1)]);

                Assert.NotEqual(array1, array2);
            }
            
            [Fact]
            public void UnequalWhenElementsAreEqualButFewer()
            {
                var array1 = new Array([new Integer(1), new Integer(2), new Integer(3)]);
                var array2 = new Array([new Integer(1), new Integer(2)]);

                Assert.NotEqual(array1, array2);
            }
            
            [Fact]
            public void UnequalWhenElementsAreEqualButMore()
            {
                var array1 = new Array([new Integer(1), new Integer(2), new Integer(3)]);
                var array2 = new Array([new Integer(1), new Integer(2), new Integer(3), new Integer(4)]);

                Assert.NotEqual(array1, array2);
            }
        }

        public class HashCode
        {
            [Fact]
            public void SameHashCodeForSameInstance()
            {
                var array = new Array([new Integer(1), new Integer(2), new Integer(3)]);

                Assert.Equal(array.GetHashCode(), array.GetHashCode());
            }

            [Fact]
            public void SameHashCodeWhenElementsAreEqual()
            {
                var array1 = new Array([new Integer(1), new Integer(2), new Integer(3)]);
                var array2 = new Array([new Integer(1), new Integer(2), new Integer(3)]);

                Assert.Equal(array1.GetHashCode(), array2.GetHashCode());
            }

            [Fact]
            public void DifferentHashCodeWhenElementsAreDifferent()
            {
                var array1 = new Array([new Integer(1), new Integer(2), new Integer(3)]);
                var array2 = new Array([new Integer(4), new Integer(5)]);

                Assert.NotEqual(array1.GetHashCode(), array2.GetHashCode());
            }

            [Fact]
            public void DifferentHashCodeWhenElementsAreEqualButFewer()
            {
                var array1 = new Array([new Integer(1), new Integer(2), new Integer(3)]);
                var array2 = new Array([new Integer(1), new Integer(2)]);

                Assert.NotEqual(array1.GetHashCode(), array2.GetHashCode());
            }

            [Fact]
            public void DifferentHashCodeWhenElementsAreEqualButMore()
            {
                var array1 = new Array([new Integer(1), new Integer(2), new Integer(3)]);
                var array2 = new Array([new Integer(1), new Integer(2), new Integer(3), new Integer(4)]);

                Assert.NotEqual(array1.GetHashCode(), array2.GetHashCode());
            }

            [Fact]
            public void DifferentHashCodeWhenElementAreEqualButOrderIsDifferent()
            {
                var array1 = new Array([new Integer(1), new Integer(2), new Integer(3)]);
                var array2 = new Array([new Integer(3), new Integer(2), new Integer(1)]);

                Assert.NotEqual(array1.GetHashCode(), array2.GetHashCode());
            }
        }
    }
}
