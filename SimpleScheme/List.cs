// <copyright file="List.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// List utilities used by the primitives.
    /// </summary>
    public sealed class List
    {
        /// <summary>
        /// Create a list from an object.
        /// Makes a list of length 1.
        /// </summary>
        /// <param name="a">The object to put into the list.</param>
        /// <returns>The Pair making up the head of the list.</returns>
        public static Pair MakeList(object a)
        {
            return new Pair(a, null);
        }

        /// <summary>
        /// Create a list from two objects.
        /// Makes a list of length 2.
        /// </summary>
        /// <param name="a">The first object.</param>
        /// <param name="b">The second object.</param>
        /// <returns>The Pair making up the head of the list.</returns>
        public static Pair MakeList(object a, object b)
        {
            return new Pair(a, new Pair(b, null));
        }

        /// <summary>
        /// Create a list made out of all the objects given.
        /// </summary>
        /// <param name="args">The object to make into a list.</param>
        /// <returns>The items appended.</returns>
        public static object ListStar(object args)
        {
            return Rest(args) == null ? 
                First(args) : 
                Cons(First(args), ListStar(Rest(args)));
        }

        /// <summary>
        /// Get the first member of a list.
        /// </summary>
        /// <param name="x">The list to use.</param>
        /// <returns>The first member of the list, or null if not a list.</returns>
        public static object First(object x)
        {
            return x is Pair ? ((Pair)x).First : null;
        }

        /// <summary>
        /// Get the second member of a list.
        /// </summary>
        /// <param name="x">The list to use.</param>
        /// <returns>The second member of the list, 
        /// or null if there is none.</returns>
        public static object Second(object x)
        {
            return First(Rest(x));
        }

        /// <summary>
        /// Get the third member of a list.
        /// </summary>
        /// <param name="x">The list to use.</param>
        /// <returns>The third member of the list, or null if there is none.</returns>
        public static object Third(object x)
        {
            return First(Rest(Rest(x)));
        }

        /// <summary>
        /// Return the rest of a list.
        /// </summary>
        /// <param name="x">The list to use.</param>
        /// <returns>The rest -- the list with the first stripped off, 
        /// or null if there is none.</returns>
        public static object Rest(object x)
        {
            return x is Pair ? ((Pair)x).Rest : null;
        }

        /// <summary>
        /// Construct a pair from two objects.
        /// </summary>
        /// <param name="a">The first object.</param>
        /// <param name="b">The rest of the objects.</param>
        /// <returns>The pair resulting from the construction.</returns>
        public static Pair Cons(object a, object b)
        {
            return new Pair(a, b);
        }

        /// <summary>
        /// Determine the length of a list.
        /// </summary>
        /// <param name="x">The list to measure.</param>
        /// <returns>The list length.</returns>
        public static int Length(object x)
        {
            int len = 0;
            if (x is Pair)
            {
                foreach (object elem in (Pair)x)
                {
                    len++;
                }
            }

            return len;
        }

        // Destructive list operations

        /// <summary>
        /// Set the first member of a pair destructively.
        /// </summary>
        /// <param name="x">The pair whose first member we want to modify.</param>
        /// <param name="y">The new value to put into it.</param>
        /// <returns>The object that has just been modified.</returns>
        public static object SetFirst(object x, object y)
        {
            return x is Pair ? 
                ((Pair)x).First = y : 
                ErrorHandlers.Error("SetFirst: attempt to set-car of a non-Pair: " + SchemeString.AsString(x));
        }

        /// <summary>
        /// Set the second member of a pair (the rest) destructively.
        /// </summary>
        /// <param name="x">The pair whose second member we want to modify.</param>
        /// <param name="y">The new value to put into it.</param>
        /// <returns>The object that has just been modified.</returns>
        public static object SetRest(object x, object y)
        {
            return x is Pair ? ((Pair)x).Rest = y : ErrorHandlers.Error("SetRest: attempt to set-cdr of a non-Pair: " + SchemeString.AsString(x));
        }

        /// <summary>
        /// Create a list containing objects in the given list in the reverse order.
        /// </summary>
        /// <param name="x">The list to reverse.</param>
        /// <returns>The reversed list.</returns>
        public static object Reverse(object x)
        {
            object result = null;
            if (x is Pair)
            {
                foreach (object elem in (Pair)x)
                {
                    result = Cons(elem, result);
                }
            }

            return result;
        }

        /// <summary>
        /// Append a list of lists, making one longer list.
        /// The appending only goes one level deep.
        /// The very last list is not copied, but is not copied, but is instead shared.
        /// </summary>
        /// <param name="args">A list of lists.  Each of the lists in this list is appended together.</param>
        /// <returns>A list of the given list elements.</returns>
        public static object Append(object args)
        {
            Pair result = MakeList(null);
            Pair accum = result;
            // TODO convert to user foreach
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
                tail.Rest = MakeList(First(toCopy));
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
            // TODO convert to user foreach
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
                        found = SchemeBoolean.Eqv(target, obj);
                        break;

                    case ' ':
                        found = SchemeBoolean.Equal(target, obj);
                        break;

                    default:
                        ErrorHandlers.Warn("Bad option to memberAssoc: " + eq);
                        return SchemeBoolean.False;
                }

                if (found)
                {
                    return m == 'm' ? list : First(list);
                }

                list = Rest(list);
            }

            return SchemeBoolean.False;
        }
    }
}
