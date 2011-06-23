// <copyright file="Pair.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;

    /// <summary>
    /// A pair consists of two cells, named First and Rest.
    /// These are used to build the linked-list structures.
    /// </summary>
    public sealed class Pair : SchemeUtils
    {
        /// <summary>
        /// Initializes a new instance of the Pair class.
        /// </summary>
        /// <param name="first">The first object.</param>
        /// <param name="rest">The rest of the objects int he list are 
        ///     referenced by this.</param>
        public Pair(object first, object rest)
        {
            this.First = first;
            this.Rest = rest;
        }

        /// <summary>
        /// Gets or sets the first ofject of the pair.
        /// </summary>
        public new object First { get; set; }

        /// <summary>
        /// Gets or sets the rest of the objects in the list.
        /// </summary>
        public new object Rest { get; set; }

        /// <summary>
        /// Tests whether the given object is equal to this pair.
        /// </summary>
        /// <param name="x">The object to test.</param>
        /// <returns>True if the given object is equal to this pair.</returns>
        public override bool Equals(object x)
        {
            if (!(x is Pair))
            {
                return false;    // TODO vectors should be tested
            }

            if (x == this)
            {
                return true;
            }

            Pair other = (Pair)x;
            return Equal(this.First, other.First) && Equal(this.Rest, other.Rest);
        }

        /// <summary>
        /// Get hash code.
        /// We have to provide this if we override Equals.
        /// </summary>
        /// <returns>The hash code.</returns>
        public override int GetHashCode()
        {
            return base.GetHashCode();
        }

        /// <summary>
        /// Turn the pair into a string for printing and such.
        /// Ultimately calls StringifyPair.
        /// </summary>
        /// <returns>A string representing the pair.</returns>
        public override string ToString()
        {
            return Stringify(this, true);
        }

        /// <summary>
        /// Turns the pair into a string.
        /// </summary>
        /// <param name="quoted">Is the string to be quoted?</param>
        /// <param name="buf">The buffer to write the string into.</param>
        public void StringifyPair(bool quoted, StringBuilder buf)
        {
            string special = null;

            if (this.Rest is Pair && SchemeUtils.Rest(this.Rest) == null)
            {
                // These is just one more thing in the pair, and the first thing 
                //    is one of these special forms.
                switch (this.First as string)
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
                Stringify(Second(this), quoted, buf);
                return;
            }

            // Normal case -- put out the whole list within parentheses.
            buf.Append('(');
            Stringify(this.First, quoted, buf);

            object tail = this.Rest;
            while (tail is Pair)
            {
                buf.Append(' ');
                Stringify(((Pair)tail).First, quoted, buf);
                tail = ((Pair)tail).Rest;
            }

            if (tail != null)
            {
                buf.Append(" . ");
                Stringify(tail, quoted, buf);
            }

            buf.Append(')');
        }
    }
}