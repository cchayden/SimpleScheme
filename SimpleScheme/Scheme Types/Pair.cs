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
    public sealed class Pair : Printable
    {
        #region Constants
        /// <summary>
        /// The printable name of the scheme pair type.
        /// </summary>
        public const string Name = "pair";
        #endregion

        #region Fields

        #endregion

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
        /// Gets the first obj of the pair.
        /// </summary>
        public object First { get; private set; }

        /// <summary>
        /// Gets the rest of the objs in the list.
        /// </summary>
        public object Rest { get; private set; }

        #endregion

        #region Public Static Methods
        /// <summary>
        /// Tests whether to given object is a scheme pair.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme pair.</returns>
        public static bool Is(Obj obj)
        {
            return obj is Pair;
        }

        /// <summary>
        /// Convert an object into a pair.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as a pair.</returns>
        public static Pair As(Obj obj)
        {
            if (Is(obj))
            {
                return (Pair)obj;
            }

            ErrorHandlers.TypeError(Name, obj);
            return null;
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
            if (!Is(obj2))
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

                if (!Is(obj1) || !Is(obj2))
                {
                    return SchemeBoolean.Equal(obj1, obj2);
                }

                pair1 = (Pair)obj1;
                pair2 = (Pair)obj2;
            }
        }

        /// <summary>
        /// Construct a pair from two objs.
        /// </summary>
        /// <param name="a">The first obj.</param>
        /// <param name="b">The rest of the objs.</param>
        /// <returns>The pair resulting from the construction.</returns>
        public static Pair Cons(Obj a, Obj b)
        {
            return new Pair(a, b);
        }
        #endregion

        #region Public Methods

        /// <summary>
        /// Destructive setter of first cell.
        /// </summary>
        /// <param name="value">The new value for the first cell.</param>
        /// <returns>The new value</returns>
        public Obj SetFirst(Obj value)
        {
            return this.First = value;
        }

        /// <summary>
        /// Destructive setter of rest cell.
        /// </summary>
        /// <param name="value">The new value for the rest cell.</param>
        /// <returns>The new value</returns>
        public Obj SetRest(Obj value)
        {
            return this.Rest = value;
        }

        /// <summary>
        /// Write the pair to the string builder.
        /// Handle some special forms separately.
        /// Otherwise, just iterate down the list printing each element.
        /// Also, detect and handle improper lists.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public override void AsString(bool quoted, StringBuilder buf)
        {
            Obj tail = List.Rest(this);
            if (Is(tail) && EmptyList.Is(List.Rest(tail)))
            {
                string special = null;

                // There is just one more thing in the pair.  See if the first thing 
                //    is one of these special forms.
                switch (List.First(this) as string)
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
                    Printer.AsString(List.Second(this), quoted, buf);
                    return;
                }
            }

            // Normal case -- put out the whole list within parentheses.
            buf.Append('(');
            Printer.AsString(List.First(this), quoted, buf);

            int len = 0;
            while (Is(tail))
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

            if (!EmptyList.Is(tail))
            {
                buf.Append(" . ");
                Printer.AsString(tail, quoted, buf);
            }

            buf.Append(')');
        }

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
}