// <copyright file="Pair.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;
    using System.Text;

    /// <summary>
    /// A pair consists of two cells, named FirstCell and RestCell.
    /// These are used to build the linked-list structures.
    /// </summary>
    public class Pair : SchemeObject
    {
        #region Fields
        /// <summary>
        /// The first object of the pair.
        /// </summary>
        private SchemeObject firstCell;

        /// <summary>
        /// The rest object of the pair.
        /// </summary>
        private SchemeObject restCell;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Pair class.
        /// </summary>
        /// <param name="first">The first object.</param>
        /// <param name="rest">The rest of the objs in the list are 
        /// referenced by this.</param>
        protected Pair(SchemeObject first, SchemeObject rest)
        {
            Contract.Requires(first != null);
            Contract.Requires(rest != null);
            Contract.Ensures(this.firstCell != null);
            Contract.Ensures(this.restCell != null);
            this.firstCell = first;
            this.restCell = rest;
        }

        /// <summary>
        /// Initializes a new instance of the Pair class.
        /// </summary>
        /// <param name="first">The first object.</param>
        /// <param name="rest">The rest of the objs in the list are 
        /// referenced by this.</param>
        /// <param name="lineNumber">The line where the Pair is read.</param>
        protected Pair(SchemeObject first, SchemeObject rest, int lineNumber) : base(lineNumber)
        {
            Contract.Requires(first != null);
            Contract.Requires(rest != null);
            Contract.Ensures(this.firstCell != null);
            Contract.Ensures(this.restCell != null);
            this.firstCell = first;
            this.restCell = rest;
        }

        /// <summary>
        /// Initializes a new instance of the Pair class.
        /// Make a one-element list.
        /// </summary>
        /// <param name="first">The first object.</param>
        protected Pair(SchemeObject first)
        {
            Contract.Requires(first != null);
            Contract.Ensures(this.firstCell != null);
            Contract.Ensures(this.restCell != null);
            this.firstCell = first;
            this.restCell = EmptyList.Instance;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Pair"/> class. 
        /// Make an empty list.
        /// </summary>
        protected Pair()
        {
            Contract.Ensures(this.firstCell != null);
            Contract.Ensures(this.restCell != null);
            this.firstCell = EmptyList.Instance;
            this.restCell = EmptyList.Instance;
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the first obj of the pair.
        /// </summary>
        internal SchemeObject FirstCell 
        {
            get
            {
                Contract.Ensures(Contract.Result<SchemeObject>() != null);
                return this.firstCell;
            }
        }

        /// <summary>
        /// Gets the rest of the objs in the list.
        /// </summary>
        internal SchemeObject RestCell
        {
            get
            {
                Contract.Ensures(Contract.Result<SchemeObject>() != null);
                return this.restCell;
            }
        }

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
            Contract.Requires(first != null);
            Contract.Ensures(Contract.Result<Pair>() != null);
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
            Contract.Requires(first != null);
            Contract.Requires(rest != null);
            Contract.Ensures(Contract.Result<Pair>() != null);
            return new Pair(first, rest);
        }

        /// <summary>
        /// Initializes a new instance of the Pair class.
        /// </summary>
        /// <param name="first">The first object.</param>
        /// <param name="rest">The rest of the objs in the list are 
        /// referenced by this.</param>
        /// <param name="lineNumber">The line number the pair was read from.</param>
        /// <returns>A new Pair.</returns>
        public static Pair New(SchemeObject first, SchemeObject rest, int lineNumber)
        {
            Contract.Requires(first != null);
            Contract.Requires(rest != null);
            Contract.Ensures(Contract.Result<Pair>() != null);
            return new Pair(first, rest, lineNumber);
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
            Contract.Requires(elem1 != null);
            Contract.Requires(elem2 != null);
            if (!(elem2 is Pair))
            {
                return SchemeBoolean.False;
            }

            var pair1 = elem1;
            var pair2 = (Pair)elem2;
            Contract.Assert(pair1 != null);
            Contract.Assert(pair2 != null);

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
                Contract.Assert(pair1 != null);
                Contract.Assert(pair2 != null);
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
            return this.ToString(true);
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Destructively reverse a list.
        /// Use this with caution: it can mess up continuations.
        /// It is safest to clone what you are going to destroy.
        /// </summary>
        /// <param name="expr">The list to reverse.</param>
        /// <returns>The reversed list.</returns>
        internal static SchemeObject ReverseListInPlace(SchemeObject expr)
        {
            Contract.Requires(expr != null);
            Contract.Ensures(Contract.Result<SchemeObject>() != null);
            SchemeObject prev = EmptyList.Instance;
            SchemeObject curr = expr;
            while (curr is Pair)
            {
                SchemeObject temp = ((Pair)curr).RestCell;
                ((Pair)curr).restCell = prev;
                prev = curr;
                curr = temp;
            }

            return prev;
        }

        /// <summary>
        /// Cleam the whole list, by cleaning each element.
        /// </summary>
        internal override void Clean()
        {
            First(this).Clean();
            var tail = Rest(this);
            while (tail is Pair)
            {
                First(tail).Clean();
                tail = Rest(tail);
            }
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Destructive setter of first cell.
        /// </summary>
        /// <param name="value">The new value for the first cell.</param>
        /// <returns>The new value</returns>
        internal SchemeObject SetFirst(SchemeObject value)
        {
            Contract.Requires(value != null);
            Contract.Ensures(Contract.Result<SchemeObject>() != null);
            return this.firstCell = value;
        }

        /// <summary>
        /// Destructive setter of rest cell.
        /// Preserves the type of the value set.
        /// </summary>
        /// <typeparam name="T">The type of the value and result.</typeparam>
        /// <param name="value">The new value for the rest cell.</param>
        /// <returns>The new value</returns>
        internal T SetRest<T>(T value) where T : SchemeObject
        {
            Contract.Requires(value != null);
            Contract.Ensures(Contract.Result<T>() != null);
            this.restCell = value;
            return value;
        }

        /// <summary>
        /// Write the pair to the string builder.
        /// Handle some special forms separately.
        /// Otherwise, just iterate down the list printing each element.
        /// Also, detect and handle improper lists.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <returns>The pair as a string.</returns>
        internal override string ToString(bool quoted)
        {
            var tail = Rest(this);
            if (tail is Pair && Rest(tail) is EmptyList)
            {
                // There is just one more thing in the pair.  See if the first thing 
                //    is one of these special forms.
                var curr = First(this);
                if (curr is Symbol)
                {
                    switch (curr.ToString())
                    {
                        case "quote":
                            return "'" + Second(this).ToString(quoted);
                        case "quasiquote":
                            return "`" + Second(this).ToString(quoted);
                        case "unquote":
                            return "," + Second(this).ToString(quoted);
                        case "unquote-splicing":
                            return ",@" + Second(this).ToString(quoted);
                    }
                }
            }

            // Normal case -- put out the whole list within parentheses.
            var buf = new StringBuilder("(");
            buf.Append(First(this).ToString(quoted));

            int len = 0;
            while (tail is Pair)
            {
                buf.Append(' ');
                buf.Append(First(tail).ToString(quoted));
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
                buf.Append(tail.ToString(quoted));
            }

            buf.Append(')');
            return buf.ToString();
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.firstCell != null);
            Contract.Invariant(this.restCell != null);
        }
        #endregion
    }
}