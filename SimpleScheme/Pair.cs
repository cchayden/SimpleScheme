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
    public sealed class Pair : SchemeUtils
    {
        /// <summary>
        /// Initializes a new instance of the Pair class.
        /// </summary>
        /// <param name="first">The first object.</param>
        /// <param name="rest">The rest of the objects int he list are 
        /// referenced by this.</param>
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
                return false;
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
        /// Ultimately calls AsString.
        /// </summary>
        /// <returns>A string representing the pair.</returns>
        public override string ToString()
        {
            return SchemeString.AsString(this, true);
        }

        /// <summary>
        /// Turns the pair into a string.
        /// </summary>
        /// <param name="quoted">Is the string to be quoted?</param>
        /// <param name="buf">The buffer to write the string into.</param>
        public void AsString(bool quoted, StringBuilder buf)
        {
            if (this.Rest is Pair && Rest(this.Rest) == null)
            {
                string special = null;

                // There is just one more thing in the pair.  See if the first thing 
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
            SchemeString.AsString(this.First, quoted, buf);

            object tail = this.Rest;

            int len = 0;
            while (tail is Pair)
            {
                buf.Append(' ');
                SchemeString.AsString(((Pair)tail).First, quoted, buf);
                object oldTail = tail;
                tail = ((Pair)tail).Rest;
                len++;
                if (tail == oldTail)
                {
                    // this is a circular structure -- truncate
                    buf.Append(" ... [circular list]");
                    tail = null;
                    break;
                }

                if (len > 1000)
                {
                    // maybe this is a circular structure -- truncate
                    buf.Append(" ... [too long]");
                    tail = null;
                    break;
                }
            }

            if (tail != null)
            {
                buf.Append(" . ");
                SchemeString.AsString(tail, quoted, buf);
            }

            buf.Append(')');
        }
    }
}