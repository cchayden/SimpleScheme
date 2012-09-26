// <copyright file="Pair.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;

    /// <summary>
    /// A pair consists of two cells, named First and Rest.
    /// These are used to build the linked-list structures.
    /// </summary>
    public sealed class Pair : IPrintable, ICleanable, ISchemeObject
    {
        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Pair class.
        /// </summary>
        /// <param name="first">The first object.</param>
        /// <param name="rest">The rest of the objs in the list are 
        /// referenced by this.</param>
        private Pair(ISchemeObject first, ISchemeObject rest)
        {
            this.First = first;
            this.Rest = rest;
        }

        /// <summary>
        /// Initializes a new instance of the Pair class.
        /// Make a one-element list.
        /// </summary>
        /// <param name="first">The first object.</param>
        private Pair(ISchemeObject first)
        {
            this.First = first;
            this.Rest = EmptyList.Instance;
        }

        /// <summary>
        /// Prevents a default instance of the <see cref="Pair"/> class from being created. 
        /// Make an empty list.
        /// </summary>
        private Pair()
        {
            this.First = EmptyList.Instance;
            this.Rest = EmptyList.Instance;
        }
        #endregion

        #region SchemeType Accessors
        /// <summary>
        /// Gets the name of the type.
        /// </summary>
        public string TypeName
        {
            get { return TypePrimitives.ValueTypeName(TypePrimitives.ValueType.Pair); }
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the first obj of the pair.
        /// </summary>
        public ISchemeObject First { get; private set; }

        /// <summary>
        /// Gets the rest of the objs in the list.
        /// </summary>
        public ISchemeObject Rest { get; private set; }
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
        public static Pair New(ISchemeObject first)
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
        public static Pair New(ISchemeObject first, ISchemeObject rest)
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
        /// <param name="obj1">The first object (must be a pair).</param>
        /// <param name="obj2">The other object.</param>
        /// <returns>True if they are both pairs and all elements are equal.</returns>
        public static SchemeBoolean Equal(ISchemeObject obj1, ISchemeObject obj2)
        {
            if (!(obj2 is Pair))
            {
                return (SchemeBoolean)false;
            }

            var pair1 = obj1.AsPair();
            var pair2 = obj2.AsPair();

            while (true)
            {
                if (!SchemeBoolean.Equal(List.First(pair1), List.First(pair2)).Value)
                {
                    return (SchemeBoolean)false;
                }

                obj1 = List.Rest(pair1);
                obj2 = List.Rest(pair2);

                if (!(obj1 is Pair) || !(obj2 is Pair))
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
        public ISchemeObject SetFirst(ISchemeObject value)
        {
            return this.First = value;
        }

        /// <summary>
        /// Destructive setter of rest cell.
        /// </summary>
        /// <param name="value">The new value for the rest cell.</param>
        /// <returns>The new value</returns>
        public ISchemeObject SetRest(ISchemeObject value)
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
            ISchemeObject tail = List.Rest(this);
            if (tail is Pair && List.Rest(tail) is EmptyList)
            {
                string special = null;

                // There is just one more thing in the pair.  See if the first thing 
                //    is one of these special forms.
                var curr = List.First(this);
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
                    Printer.PrintString(List.Second(this), quoted, buf);
                    return;
                }
            }

            // Normal case -- put out the whole list within parentheses.
            buf.Append('(');
            Printer.PrintString(List.First(this), quoted, buf);

            int len = 0;
            while (tail is Pair)
            {
                buf.Append(' ');
                Printer.PrintString(List.First(tail), quoted, buf);
                ISchemeObject oldTail = tail;
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

            if (!(tail is EmptyList))
            {
                buf.Append(" . ");
                Printer.PrintString(tail, quoted, buf);
            }

            buf.Append(')');
        }

        /// <summary>
        /// Cleam the whole list, by cleaning each element.
        /// </summary>
        public void Clean()
        {
            Cleaner.Clean(List.First(this));
            var tail = List.Rest(this);
            while (tail is Pair)
            {
                Cleaner.Clean(List.First(tail));
                tail = List.Rest(tail);
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

    #region Extension Class
    /// <summary>
    /// Extension for Pair
    /// </summary>
    public static class PairExtension
    {
        /// <summary>
        /// Convert an object into a pair.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as a pair.</returns>
        public static Pair AsPair(this ISchemeObject obj)
        {
            if (obj is Pair)
            {
                return (Pair)obj;
            }

            ErrorHandlers.TypeError(typeof(Pair), obj);
            return null;
        }
    }
    #endregion
}