// <copyright file="ISchemeType.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// All scheme types implement this interface.
    /// </summary>
    interface ISchemeType
    {
        /// <summary>
        /// Gets the type name.
        /// </summary>
        string TypeName { get; }
    }
}
