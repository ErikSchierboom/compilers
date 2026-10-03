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

        /// <summary>
        /// Rotate a sequence to the left.
        /// </summary>
        /// <remarks>
        /// The first element becomes the last element.
        /// All the other elements are shifted one position to the left.
        /// </remarks>
        /// <returns>The rotated sequence.</returns>
        public IEnumerable<T> RotateLeft()
        {
            using var enumerator = enumerable.GetEnumerator();

            if (!enumerator.MoveNext())
                yield break;

            var first = enumerator.Current;

            while (enumerator.MoveNext())
                yield return enumerator.Current;

            yield return first;
        }
    }
}
