namespace Arya;

public static class EnumerableExtensions
{
    extension<T>(IEnumerable<T> enumerable)
    {
        /// <summary>
        /// Repeats the elements of the sequence indefinitely.
        /// </summary>
        /// <returns>An infinite sequence of the sequence's elements.</returns>
        public IEnumerable<T> Repeat()
        {
            using var enumerator = enumerable.GetEnumerator();

            // Prevent looping endlessly when the enumerator has no elements
            if (!enumerator.MoveNext())
                yield break;
            
            while (true)
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