// <copyright file="ICleanable.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// An ICleanable class is one that has an Clean method.
    /// This command cleans out any symbol access information.
    /// </summary>
    public interface ICleanable
    {
        /// <summary>
        /// Clean the boject.
        /// </summary>
        void Clean();
    }
}
