// <copyright file="Printable.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;

    /// <summary>
    /// A printable class is one that has an PrintString method, 
    ///   which appends a printable representation to a string builder.
    /// </summary>
    public abstract class Printable
    {
        /// <summary>
        /// Print the value into the given buffer.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public abstract void PrintString(bool quoted, StringBuilder buf);
    }
}
