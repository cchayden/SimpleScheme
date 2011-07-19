// <copyright file="Undefined.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Represents an undefined scheme value.
    /// </summary>
    public class Undefined
    {
        #region Fields
        /// <summary>
        /// Keep one instance of this around to use when needed.
        /// </summary>
        public static readonly Undefined Instance = new Undefined();
        #endregion

        #region Constructor
        /// <summary>
        /// Prevents a default instance of the Undefined class from being created.
        /// </summary>
        private Undefined()
        {
        }
        #endregion
    }
}
