// <copyright file="Printable.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;

    /// <summary>
    /// A printable class is one that has an AsString method, that let's it
    ///   be converted to a string for printing.
    /// </summary>
    public abstract class Printable
    {
        /// <summary>
        /// Print the value into the given buffer.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public abstract void AsString(bool quoted, StringBuilder buf);
    }
}
