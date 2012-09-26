// <copyright file="Printable.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;

    /// <summary>
    /// A Cleanable class is one that has an Clean method.
    /// This command cleans out any symbol access information.
    /// </summary>
    public interface Cleanable
    {
        /// <summary>
        /// Clean the boject.
        /// </summary>
        void Clean();
    }
}
