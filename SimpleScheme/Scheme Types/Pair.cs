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
    public sealed class Pair : Printable, Cleanable
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
            this.Rest = EmptyList.New();
        }

        /// <summary>
        /// Initializes a new instance of the Pair class.
        /// Make an empty list.
        /// </summary>
        public Pair()
        {
            this.First = EmptyList.New();
            this.Rest = EmptyList.New();
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
        /// Tests whether two pairs are equal.
        /// The first object must be a pair.
        /// If the list is circulr, this will loop forever.
        /// </summary>
        /// <param name="obj1">The first object (must be a pair).</param>
        /// <param name="obj2">The other object.</param>
        /// <returns>True if they are both pairs and all elements are equal.</returns>
        public static bool Equal(Obj obj1, Obj obj2)
        {
            if (!obj2.IsPair())
            {
                return false;
            }

            var pair1 = obj1.AsPair();
            var pair2 = obj2.AsPair();

            while (true)
            {
                if (!SchemeBoolean.Equal(pair1.First(), pair2.First()))
                {
                    return false;
                }

                obj1 = pair1.Rest();
                obj2 = pair2.Rest();

                if (!obj1.IsPair() || !obj2.IsPair())
                {
                    return SchemeBoolean.Equal(obj1, obj2);
                }

                pair1 = obj1.AsPair();
                pair2 = obj2.AsPair();
            }
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
        public void PrintString(bool quoted, StringBuilder buf)
        {
            Obj tail = this.Rest();
            if (tail.IsPair() && tail.Rest().IsEmptyList())
            {
                string special = null;

                // There is just one more thing in the pair.  See if the first thing 
                //    is one of these special forms.
                var curr = this.First();
                if (curr.IsSymbol())
                {
                    switch (curr.ToString())
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
                }

                if (special != null)
                {
                    // There was a special form, and one more thing.
                    // Append a special symbol and the remaining thing.
                    buf.Append(special);
                    Printer.PrintString(this.Second(), quoted, buf);
                    return;
                }
            }

            // Normal case -- put out the whole list within parentheses.
            buf.Append('(');
            Printer.PrintString(this.First(), quoted, buf);

            int len = 0;
            while (tail.IsPair())
            {
                buf.Append(' ');
                Printer.PrintString(tail.First(), quoted, buf);
                Obj oldTail = tail;
                tail = tail.Rest();
                len++;
                if (tail == oldTail)
                {
                    // this is a circular structure -- truncate
                    buf.Append(" ... [circular list]");
                    tail = EmptyList.New();
                    break;
                }

                if (len > 1000)
                {
                    // maybe this is a circular structure -- truncate
                    buf.Append(" ... [too long]");
                    tail = EmptyList.New();
                    break;
                }
            }

            if (!tail.IsEmptyList())
            {
                buf.Append(" . ");
                Printer.PrintString(tail, quoted, buf);
            }

            buf.Append(')');
        }

        public void Clean()
        {
            Cleaner.Clean(this.First());
            Obj tail = this.Rest();
            while (tail.IsPair())
            {
                Cleaner.Clean(tail.First());
                Obj oldTail = tail;
                tail = tail.Rest();
            }
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

    /// <summary>
    /// Extension for Pair
    /// </summary>
    public static class PairExtensions
    {
        /// <summary>
        /// Tests whether to given object is a scheme pair.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme pair.</returns>
        public static bool IsPair(this Obj obj)
        {
            return obj is Pair;
        }

        /// <summary>
        /// Convert an object into a pair.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as a pair.</returns>
        public static Pair AsPair(this Obj obj)
        {
            if (obj.IsPair())
            {
                return (Pair)obj;
            }

            ErrorHandlers.TypeError(Pair.Name, obj);
            return null;
        }
    }
}