// <copyright file="ISchemeObject.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// All scheme types implement this interface.
    /// </summary>
    public interface ISchemeObject
    {
        /// <summary>
        /// Gets the type name.
        /// </summary>
        string TypeName { get; }
    }
}

// TODO cch Merge with IPrintable