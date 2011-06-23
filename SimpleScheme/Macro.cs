﻿// <copyright file="Macro.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Represents a macro definition.
    /// It is just a closure with a different ToString.
    /// </summary>
    internal sealed class Macro : Closure
    {
        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Macro class.
        /// </summary>
        /// <param name="parms">The macro params.</param>
        /// <param name="body">The macro body.</param>
        /// <param name="env">The environment that the macro is defined in.</param>
        private Macro(Obj parms, Obj body, Environment env)
            : base(parms, body, env)
        {
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Creates a new Macro.
        /// </summary>
        /// <param name="parms">The macro params.</param>
        /// <param name="body">The macro body.</param>
        /// <param name="env">The environment that the macro is defined in.</param>
        /// <returns>A new instance of Macro class.</returns>
        internal static new Macro New(Obj parms, Obj body, Environment env)
        {
            return new Macro(parms, body, env);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Display the macro as a string.  
        /// Displays the formal parameters and the body, as it has been processed by the reader.
        /// </summary>
        /// <returns>The string form of the closure.</returns>
        public override string ToString()
        {
            return this.ToString("macro");
        }
        #endregion
    }
}