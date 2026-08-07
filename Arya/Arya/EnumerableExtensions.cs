namespace Arya;

public static class EnumerableExtensions
{
    extension<T>(IEnumerable<T> enumerable)
    {
        public IEnumerable<T> Cycle(int length)
        {
            using var enumerator = enumerable.GetEnumerator();

            // Prevent looping endlessly when the enumerator has no elements
            if (!enumerator.MoveNext())
                yield break;
            
            for (var i = 0; i < length; i++)
            {
                yield return enumerator.Current;
                
                // When we've reached the end, reset the enumerator
                // to its initial position to keep on iterating
                if (!enumerator.MoveNext())
                {
                    enumerator.Reset();
                    enumerator.MoveNext();
                }
            }
        }
    }
}