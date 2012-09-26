// <copyright file="Macro.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Represents a macro definition.
    /// It is just a lambda with a different ToString.
    /// It is effectively immutable.
    /// </summary>
    internal sealed class Macro : Lambda
    {
        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Macro class.
        /// </summary>
        /// <param name="parms">The macro params.</param>
        /// <param name="body">The macro body.</param>
        /// <param name="env">The environment that the macro is defined in.</param>
        public Macro(SchemeObject parms, SchemeObject body, Environment env)
            : base(parms, body, env)
        {
            Contract.Requires(parms != null);
            Contract.Requires(body != null);
            Contract.Requires(env != null);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Display the macro as a string.  
        /// Displays the formal parameters and the body, as it has been processed by the reader.
        /// </summary>
        /// <returns>The string form of the lambda.</returns>
        public override string ToString()
        {
            return this.ToString("macro");
        }
        #endregion

        #region Call
        /// <summary>
        /// Evaluate a macro.
        /// </summary>
        /// <param name="args">The macro args.</param>
        /// <param name="env">The execution environment.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The macro representing the expression.</returns>
        internal static new Evaluator Call(SchemeObject args, Environment env, Evaluator caller)
        {
            Contract.Requires(args != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            caller.ReturnedExpr = new Macro(First(args), Rest(args), env);
            return caller;
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Evaluate the macro.
        /// At this point, the args are NOT evaluated.
        /// Expand the macro and then continue.
        /// </summary>
        /// <param name="args">The macro arguments.</param>
        /// <param name="env">The environment to use for the application.</param>
        /// <param name="caller">Return here when done.</param>
        /// <returns>The next evaluator to execute.</returns>
        internal override Evaluator Evaluate(SchemeObject args, Environment env, Evaluator caller)
        {
            return EvaluateExpandMacro.Call(this, args, env, caller);
        }
        #endregion
    }
}