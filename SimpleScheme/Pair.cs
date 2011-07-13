// <copyright file="Pair.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// A pair consists of two cells, named FirstCell and RestCell.
    /// These are used to build the linked-list structures.
    /// </summary>
    public sealed class Pair : ListPrimitives
    {
        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Pair class.
        /// </summary>
        /// <param name="first">The first object.</param>
        /// <param name="rest">The rest of the objs in the list are 
        /// referenced by this.</param>
        private Pair(Obj first, Obj rest)
        {
            this.FirstCell = first;
            this.RestCell = rest;
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets or sets the first obj of the pair.
        /// </summary>
        internal Obj FirstCell { get; set; }

        /// <summary>
        /// Gets or sets the rest of the objs in the list.
        /// </summary>
        internal Obj RestCell { get; set; }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Create a new pair.
        /// </summary>
        /// <param name="first">The first object.</param>
        /// <param name="rest">The rest of the objs in the list are 
        /// referenced by this.</param>
        /// <returns>The new pair.</returns>
        public static Pair New(Obj first, Obj rest)
        {
            return new Pair(first, rest);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Turn the pair into a string for printing and such.
        /// Ultimately calls AsString.
        /// </summary>
        /// <returns>A string representing the pair.</returns>
        public override string ToString()
        {
            return SchemeString.AsString(this, true);
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Test an object's type.
        /// </summary>
        /// <param name="obj">The object to test.</param>
        /// <returns>True if the object is a scheme pair.</returns>
        internal static bool IsType(Obj obj)
        {
            return obj is Pair;
        }

        /// <summary>
        /// Give the name of the type (for display).
        /// </summary>
        /// <returns>The type name.</returns>
        internal static string TypeName()
        {
            return "pair";
        }

        /// <summary>
        /// Tests whether two pairs are equal.
        /// </summary>
        /// <param name="obj1">The first object (must be a pair).</param>
        /// <param name="obj2">The other object.</param>
        /// <returns>True if they are both pairs and 
        /// all elements are equal.</returns>
        internal static bool Equal(Obj obj1, Obj obj2)
        {
            if (!(obj2 is Pair))
            {
                return false;
            }

            if (obj1 == obj2)
            {
                return true;
            }

            Pair pair1 = (Pair)obj1;
            Pair pair2 = (Pair)obj2;

            return SchemeBoolean.Equal(First(pair1), First(pair2)) && SchemeBoolean.Equal(Rest(pair1), Rest(pair2));
        }

        /// <summary>
        /// Turns the pair into a string.
        /// </summary>
        /// <param name="quoted">Is the string to be quoted?</param>
        /// <param name="buf">The buffer to write the string into.</param>
        internal void AsString(bool quoted, StringBuilder buf)
        {
            if (this.RestCell is Pair && Rest(this.RestCell) == List.Empty)
            {
                string special = null;

                // There is just one more thing in the pair.  See if the first thing 
                //    is one of these special forms.
                switch (this.FirstCell as string)
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
                    SchemeString.AsString(Second(this), quoted, buf);
                    return;
                }
            }

            // Normal case -- put out the whole list within parentheses.
            buf.Append('(');
            SchemeString.AsString(this.FirstCell, quoted, buf);

            Obj tail = this.RestCell;

            int len = 0;
            while (tail is Pair)
            {
                buf.Append(' ');
                SchemeString.AsString(First(tail), quoted, buf);
                Obj oldTail = tail;
                tail = Rest(tail);
                len++;
                if (tail == oldTail)
                {
                    // this is a circular structure -- truncate
                    buf.Append(" ... [circular list]");
                    tail = List.Empty;
                    break;
                }

                if (len > 1000)
                {
                    // maybe this is a circular structure -- truncate
                    buf.Append(" ... [too long]");
                    tail = List.Empty;
                    break;
                }
            }

            if (tail != List.Empty)
            {
                buf.Append(" . ");
                SchemeString.AsString(tail, quoted, buf);
            }

            buf.Append(')');
        }
        #endregion
    }
}