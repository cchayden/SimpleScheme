#define OLDxx
// <copyright file="Macro.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// Represents a macro definition.
    /// </summary>
    public sealed class Macro : Closure
    {
        /// <summary>
        /// Initializes a new instance of the Macro class.
        /// </summary>
        /// <param name="parms">The macro params.</param>
        /// <param name="body">The macro body.</param>
        /// <param name="env">The environment that the macro is defined in.</param>
        public Macro(object parms, object body, Environment env)
            : base(parms, body, env)
        {
        }

        /// <summary>
        /// Expand the macro.
        /// </summary>
        /// <param name="interpreter">The interpreter used in the expansion.</param>
        /// <param name="parent">The calling Evaluator.</param>
        /// <param name="oldPair">The expression to replace by the expansion.</param>
        /// <param name="args">The macro args.</param>
        /// <returns>The result of expanding the macro.</returns>
        public object Expand(Scheme interpreter, Evaluator parent, object args)
        {
            return Apply(interpreter, parent, args);
        }
    }
}