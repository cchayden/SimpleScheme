﻿// <copyright file="Macro.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
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
        internal Macro(Obj parms, Obj body, Environment env)
            : base(parms, body, env)
        {
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Tests whether to given object is a scheme macro.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme macro.</returns>
        public static bool IsMacro(Obj obj)
        {
            return obj is Macro;
        }

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

        #region Internal Methods
        /// <summary>
        /// Convert object to macro.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as a macro.</returns>
        internal static Macro AsMacro(Obj obj)
        {
            return (Macro)obj;
        }
        #endregion
    }

    /// <summary>
    /// Provide common operations as extensions.
    /// </summary>
    internal static partial class Extensions
    {
        /// <summary>
        /// Write the macro to the string builder.
        /// </summary>
        /// <param name="macro">The macro.</param>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        internal static void AsString(this Macro macro, bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append("macro: ");
                buf.Append(macro.ToString());
            }
        }
    }
}