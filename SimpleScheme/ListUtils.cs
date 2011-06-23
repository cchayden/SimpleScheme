// <copyright file="ListUtils.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// List utilities used by the primitives.
    /// </summary>
    public class ListUtils : SchemeUtils
    {
        /// <summary>
        /// Append a list of lists, making one longer list.
        /// The appending only goes one level deep.
        /// The very last list is not copied, but is not copied, but is instead shared.
        /// </summary>
        /// <param name="args">A list of lists.  Each of the lists in this list is appended together.</param>
        /// <returns>A list of the given list elements.</returns>
        public static object Append(object args)
        {
            Pair result = List(null);
            Pair accum = result;
            while (Rest(args) != null)
            {
                accum = Append(accum, First(args));
                args = Rest(args);
            }

            accum.Rest = First(args);
            return Rest(result);
        }

        /// <summary>
        /// Append one list to the tail of another.
        /// The tail is modified destructively.
        /// The appended list is copied.
        /// </summary>
        /// <param name="tail">The end of the first list, destructively appended to.</param>
        /// <param name="toCopy">The second list, copied onto the first.</param>
        /// <returns>The end of the second list, suitable for another call to this function. </returns>
        public static Pair Append(Pair tail, object toCopy)
        {
            while (toCopy != null)
            {
                tail.Rest = List(First(toCopy));
                toCopy = Rest(toCopy);
                tail = (Pair)Rest(tail);
            }

            return tail;
        }

        /// <summary>
        /// Tests to see if the given object is a list.
        /// </summary>
        /// <param name="x">The object to test.</param>
        /// <returns>True if the object is a list.</returns>
        public static bool IsList(object x)
        {
            while (true)
            {
                if (x == null)
                {
                    return true;
                }

                if (!(x is Pair))
                {
                    return false;
                }

                object rest = Rest(x);
                if (rest == x)
                {
                    return false;
                }

                x = rest;
            }
        }

        /// <summary>
        /// Searches lists, used by memq, memv, and member.
        /// Also used by assq, assv, and assoc.
        /// </summary>
        /// <param name="obj">The object to search for.</param>
        /// <param name="list">The list to search in.</param>
        /// <param name="m">If 'm', do member, if 'a' do assoc.</param>
        /// <param name="eq">This gives the type of equality test to use.</param>
        /// <returns>The results that wer found.</returns>
        public static object MemberAssoc(object obj, object list, char m, char eq)
        {
            while (list is Pair)
            {
                object target = m == 'm' ? First(list) : First(First(list));
                bool found;
                switch (eq)
                {
                    case 'q':
                        found = target == obj;
                        break;

                    case 'v':
                        found = Eqv(target, obj);
                        break;

                    case ' ':
                        found = Equal(target, obj);
                        break;

                    default:
                        Warn("Bad option to memberAssoc: " + eq);
                        return False;
                }

                if (found)
                {
                    return m == 'm' ? list : First(list);
                }

                list = Rest(list);
            }

            return False;
        }
    }
}
