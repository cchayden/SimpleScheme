// <copyright file="Pair.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// A pair consists of two cells, named First and Rest.
    /// These are used to build the linked-list structures.
    /// </summary>
    public sealed class Pair
    {
        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Pair class.
        /// </summary>
        /// <param name="first">The first object.</param>
        /// <param name="rest">The rest of the objs in the list are 
        /// referenced by this.</param>
        public Pair(Obj first, Obj rest)
        {
            this.First = first;
            this.Rest = rest;
        }

        /// <summary>
        /// Initializes a new instance of the Pair class.
        /// Make a one-element list.
        /// </summary>
        /// <param name="first">The first object.</param>
        public Pair(Obj first)
        {
            this.First = first;
            this.Rest = EmptyList.Instance;
        }

        /// <summary>
        /// Initializes a new instance of the Pair class.
        /// Make an empty list.
        /// </summary>
        public Pair()
        {
            this.First = EmptyList.Instance;
            this.Rest = EmptyList.Instance;
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets or sets the first obj of the pair.
        /// </summary>
        public Obj First { get; set; }

        /// <summary>
        /// Gets or sets the rest of the objs in the list.
        /// </summary>
        public Obj Rest { get; set; }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Tests whether to given object is a scheme pair.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme pair.</returns>
        public static bool IsPair(Obj obj)
        {
            return obj is Pair;
        }

        /// <summary>
        /// Convert an object into a pair.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as a pair.</returns>
        public static Pair AsPair(Obj obj)
        {
            return (Pair)obj;
        }

        /// <summary>
        /// Tests whether two pairs are equal.
        /// The first object must be a pair.
        /// If the list is circulr, this will loop forever.
        /// </summary>
        /// <param name="obj1">The first object (must be a pair).</param>
        /// <param name="obj2">The other object.</param>
        /// <returns>True if they are both pairs and all elements are equal.</returns>
        public static bool Equal(Obj obj1, Obj obj2)
        {
            if (!IsPair(obj2))
            {
                return false;
            }

            Pair pair1 = (Pair)obj1;
            Pair pair2 = (Pair)obj2;

            while (true)
            {
                if (!SchemeBoolean.Equal(List.First(pair1), List.First(pair2)))
                {
                    return false;
                }

                obj1 = List.Rest(pair1);
                obj2 = List.Rest(pair2);

                if (!IsPair(obj1) || !IsPair(obj2))
                {
                    return SchemeBoolean.Equal(obj1, obj2);
                }

                pair1 = (Pair)obj1;
                pair2 = (Pair)obj2;
            }
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Turn the pair into a string for display.
        /// </summary>
        /// <returns>A string representing the pair.</returns>
        public override string ToString()
        {
            return Printer.AsString(this, true);
        }
        #endregion
    }

    /// <summary>
    /// Provide common operations as extensions.
    /// </summary>
    public static partial class Extensions
    {
        /// <summary>
        /// Write the pair to the string builder.
        /// Handle some special forms separately.
        /// Otherwise, just iterate down the list printing each element.
        /// Also, detect and handle improper lists.
        /// </summary>
        /// <param name="pair">The pair to print.</param>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public static void AsString(this Pair pair, bool quoted, StringBuilder buf)
        {
            if (Pair.IsPair(List.Rest(pair)) && 
                EmptyList.IsEmptyList(List.Rest(List.Rest(pair))))
            {
                string special = null;

                // There is just one more thing in the pair.  See if the first thing 
                //    is one of these special forms.
                switch (List.First(pair) as string)
                {
                    case "quote":
                        special = "'";
                        break;
                    case "quasiquote":
                        special = "`";
                        break;
                    case "unquote":
                        special = ",";
                        break;
                    case "unquote-splicing":
                        special = ",@";
                        break;
                }

                if (special != null)
                {
                    // There was a special form, and one more thing.
                    // Append a special symbol and the remaining thing.
                    buf.Append(special);
                    Printer.AsString(List.Second(pair), quoted, buf);
                    return;
                }
            }

            // Normal case -- put out the whole list within parentheses.
            buf.Append('(');
            Printer.AsString(List.First(pair), quoted, buf);

            Obj tail = List.Rest(pair);

            int len = 0;
            while (Pair.IsPair(tail))
            {
                buf.Append(' ');
                Printer.AsString(List.First(tail), quoted, buf);
                Obj oldTail = tail;
                tail = List.Rest(tail);
                len++;
                if (tail == oldTail)
                {
                    // this is a circular structure -- truncate
                    buf.Append(" ... [circular list]");
                    tail = EmptyList.Instance;
                    break;
                }

                if (len > 1000)
                {
                    // maybe this is a circular structure -- truncate
                    buf.Append(" ... [too long]");
                    tail = EmptyList.Instance;
                    break;
                }
            }

            if (!EmptyList.IsEmptyList(tail))
            {
                buf.Append(" . ");
                Printer.AsString(tail, quoted, buf);
            }

            buf.Append(')');
        }
    }
}