// <copyright file="Macro.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Represents a macro definition.
    /// It is just a lambda with a different ToString.
    /// It is effectively immutable.
    /// </summary>
    public sealed class Macro : Lambda
    {
        #region Constants
        /// <summary>
        /// The printable name of the scheme macro type.
        /// </summary>
        public new const string Name = "macro";
        #endregion

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

        #region Public Static Methods
        /// <summary>
        /// Creates a new instance of the Macro class.
        /// </summary>
        /// <param name="parms">The macro params.</param>
        /// <param name="body">The macro body.</param>
        /// <param name="env">The environment that the macro is defined in.</param>
        /// <returns>A new macro.</returns>
        public static new Macro New(Obj parms, Obj body, Environment env)
        {
            return new Macro(parms, body, env);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write the macro to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public override void PrintString(bool quoted, StringBuilder buf)
        {
            buf.Append(this.ToString());
        }

        /// <summary>
        /// Evaluate the macro.
        /// At this point, the args are NOT evaluated.
        /// Expand the macro and then continue.
        /// </summary>
        /// <param name="args">The macro arguments.</param>
        /// <param name="env">The environment to use for the application.</param>
        /// <param name="caller">Return here when done.</param>
        /// <returns>The next evaluator to execute.</returns>
        public override Evaluator Evaluate(Obj args, Environment env, Evaluator caller)
        {
            return EvaluateExpandMacro.Call(this, args, env, caller);
        }

        /// <summary>
        /// Display the macro as a string.  
        /// Displays the formal parameters and the body, as it has been processed by the reader.
        /// </summary>
        /// <returns>The string form of the lambda.</returns>
        public override string ToString()
        {
            return this.ToString(Name);
        }
        #endregion
    }

    /// <summary>
    /// Extensions for Macro
    /// </summary>
    public static class MacroExtensions
    {
        /// <summary>
        /// Tests whether to given object is a scheme macro.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme macro.</returns>
        public static bool IsMacro(this Obj obj)
        {
            return obj is Macro;
        }

        /// <summary>
        /// Convert object to macro.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as a macro.</returns>
        public static Macro AsMacro(Obj obj)
        {
            if (obj.IsMacro())
            {
                return (Macro)obj;
            }

            ErrorHandlers.TypeError(Macro.Name, obj);
            return null;
        }
    }
}