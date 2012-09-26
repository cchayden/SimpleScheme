// <copyright file="Pair.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;

    /// <summary>
    /// A pair consists of two cells, named FirstCell and RestCell.
    /// These are used to build the linked-list structures.
    /// </summary>
    public class Pair : SchemeObject, ICleanable
    {
        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Pair class.
        /// </summary>
        /// <param name="first">The first object.</param>
        /// <param name="rest">The rest of the objs in the list are 
        /// referenced by this.</param>
        protected Pair(SchemeObject first, SchemeObject rest)
        {
            this.FirstCell = first;
            this.RestCell = rest;
        }

        /// <summary>
        /// Initializes a new instance of the Pair class.
        /// Make a one-element list.
        /// </summary>
        /// <param name="first">The first object.</param>
        protected Pair(SchemeObject first)
        {
            this.FirstCell = first;
            this.RestCell = EmptyList.Instance;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Pair"/> class. 
        /// Make an empty list.
        /// </summary>
        protected Pair()
        {
            this.FirstCell = EmptyList.Instance;
            this.RestCell = EmptyList.Instance;
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the first obj of the pair.
        /// </summary>
        public SchemeObject FirstCell { get; private set; }

        /// <summary>
        /// Gets the rest of the objs in the list.
        /// </summary>
        public SchemeObject RestCell { get; private set; }
        #endregion

        #region New
        /// <summary>
        /// Initializes a new instance of the Pair class.
        /// Make an empty list.
        /// </summary>
        /// <returns>A new Pair.</returns>
        public static Pair New()
        {
            return new Pair();
        }

        /// <summary>
        /// Initializes a new instance of the Pair class.
        /// Make a one-element list.
        /// </summary>
        /// <param name="first">The first object.</param>
        /// <returns>A new Pair.</returns>
        public static Pair New(SchemeObject first)
        {
            return new Pair(first);
        }

        /// <summary>
        /// Initializes a new instance of the Pair class.
        /// </summary>
        /// <param name="first">The first object.</param>
        /// <param name="rest">The rest of the objs in the list are 
        /// referenced by this.</param>
        /// <returns>A new Pair.</returns>
        public static Pair New(SchemeObject first, SchemeObject rest)
        {
            return new Pair(first, rest);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Tests whether two pairs are equal.
        /// The first object must be a pair.
        /// If the list is circulr, this will loop forever.
        /// </summary>
        /// <param name="elem1">The first object (must be a pair).</param>
        /// <param name="elem2">The other object.</param>
        /// <returns>True if they are both pairs and all elements are equal.</returns>
        public static SchemeBoolean Equal(Pair elem1, SchemeObject elem2)
        {
            if (!(elem1 is Pair) || !(elem2 is Pair))
            {
                return SchemeBoolean.False;
            }

            var pair1 = elem1;
            var pair2 = (Pair)elem2;

            while (true)
            {
                if (!SchemeBoolean.Equal(First(pair1), First(pair2)).Value)
                {
                    return SchemeBoolean.False;
                }

                var obj1 = Rest(pair1);
                var obj2 = Rest(pair2);

                if (!(obj1 is Pair) || !(obj2 is Pair))
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
        /// Destructive setter of first cell.
        /// </summary>
        /// <param name="value">The new value for the first cell.</param>
        /// <returns>The new value</returns>
        public SchemeObject SetFirst(SchemeObject value)
        {
            return this.FirstCell = value;
        }

        /// <summary>
        /// Destructive setter of rest cell.
        /// Preserves the type of the value set.
        /// </summary>
        /// <typeparam name="T">The type of the value and result.</typeparam>
        /// <param name="value">The new value for the rest cell.</param>
        /// <returns>The new value</returns>
        public T SetRest<T>(T value) where T : SchemeObject
        {
            this.RestCell = value;
            return value;
        }

        /// <summary>
        /// Write the pair to the string builder.
        /// Handle some special forms separately.
        /// Otherwise, just iterate down the list printing each element.
        /// Also, detect and handle improper lists.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public override void PrintString(bool quoted, StringBuilder buf)
        {
            SchemeObject tail = Rest(this);
            if (tail is Pair && Rest(tail) is EmptyList)
            {
                string special = null;

                // There is just one more thing in the pair.  See if the first thing 
                //    is one of these special forms.
                var curr = First(this);
                if (curr is Symbol)
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
                    Second(this).PrintString(quoted, buf);
                    return;
                }
            }

            // Normal case -- put out the whole list within parentheses.
            buf.Append('(');
            First(this).PrintString(quoted, buf);

            int len = 0;
            while (tail is Pair)
            {
                buf.Append(' ');
                First(tail).PrintString(quoted, buf);
                SchemeObject oldTail = tail;
                tail = Rest(tail);
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

            if (!(tail is EmptyList))
            {
                buf.Append(" . ");
                tail.PrintString(quoted, buf);
            }

            buf.Append(')');
        }

        /// <summary>
        /// Cleam the whole list, by cleaning each element.
        /// </summary>
        public void Clean()
        {
            Cleaner.Clean(First(this));
            var tail = Rest(this);
            while (tail is Pair)
            {
                Cleaner.Clean(First(tail));
                tail = Rest(tail);
            }
        }

        /// <summary>
        /// Turn the pair into a string for display.
        /// </summary>
        /// <returns>A string representing the pair.</returns>
        public override string ToString()
        {
            return this.ToString(true);
        }
        #endregion
    }
}