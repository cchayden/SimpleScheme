// <copyright file="Closure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// A closure stores the environment and a program to run.
    /// It can be executed later, through Apply.
    /// The Macro class also derives from this.
    /// </summary>
    //// <r4rs section="4.1.4">(lambda <formals> <body>)</r4rs>
    //// <r4rs section="4.1.4">body: expression ...)</r4rs>
    //// <r4rs section="4.1.4">formals: (<variable1> ...)</r4rs>
    //// <r4rs section="4.1.4">formals: <variable></r4rs>
    //// <r4rs section="4.1.4">formals: (<variable 1> ... <variable n-1> . <variable n>)</r4rs>
    internal class Closure : Procedure
    {
        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Closure class.
        /// </summary>
        /// <param name="formalParameters">A list of variable names, to be matched with 
        ///    values given later.</param>
        /// <param name="body">The program to execute.</param>
        /// <param name="env">The environment in which to execute it.</param>
        internal Closure(Obj formalParameters, Obj body, Environment env)
        {
            this.FormalParameters = formalParameters;
            this.Env = env;
            this.Body = body;
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets a list of variable names, to be matched with values later.
        /// </summary>
        internal Obj FormalParameters { get; private set; }

        /// <summary>
        /// Gets the program to execute.
        /// </summary>
        internal Obj Body { get; private set; }

        /// <summary>
        /// Gets the environment in which to execute.
        /// </summary>
        internal Environment Env { get; private set; }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Tests whether to given object is a scheme closure.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme closure.</returns>
        public static bool IsClosure(Obj obj)
        {
            return obj is Closure;
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Display the closure as a string.  
        /// Displays the formal parameters and the body, as it has been processed by the reader.
        /// </summary>
        /// <returns>The string form of the closure.</returns>
        public override string ToString()
        {
            return this.ToString("lambda");
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Convert object to closure.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as a closure.</returns>
        internal static Closure AsClosure(Obj obj)
        {
            return (Closure)obj;
        }

        /// <summary>
        /// Actually executes the saved program
        /// Uses the given environment rather than creating a new one.
        /// The arguments have already been bound to the formal parameters.
        /// If there is only one expression in the body, just evaluate it, otherwise evaluate the sequence.
        /// </summary>
        /// <param name="env">The environment to evaluate in.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next step to execute.</returns>
        internal Stepper ApplyWithtEnv(Environment env, Stepper caller)
        {
            return EmptyList.IsEmptyList(List.Rest(this.Body)) ? 
                EvaluateExpression.Call(List.First(this.Body), env, caller) : 
                EvaluateSequence.Call(this.Body, env, caller);
        }

        /// <summary>
        /// Actually executes the saved program, with the given arguments bound with the 
        ///   formal parameters of the closure.
        /// Creates a new environment, linked to the encironment of the closure itself (which should be the lexical parent).
        /// </summary>
        /// <param name="args">The values to be matched with the variable names.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next step to execute.</returns>
        internal override Stepper Apply(object args, Stepper caller)
        {
            return this.ApplyWithtEnv(new Environment(this.FormalParameters, args, this.Env), caller);
        }

        /// <summary>
        /// Common printing logic for closures.
        /// </summary>
        /// <param name="tag">The function name.</param>
        /// <returns>String representing the closure.</returns>
        protected string ToString(string tag)
        {
            // trim enclosing parentheses off body, to match the definition
            string body = this.Body.ToString();
            if (body.StartsWith("(") && body.EndsWith(")"))
            {
                body = body.Substring(1, body.Length - 2).Trim();
            }

            return String.Format("({0} {1} {2})", tag, this.FormalParameters, body);
        }
        #endregion
    }

    /// <summary>
    /// Provide common operations as extensions.
    /// </summary>
    internal static partial class Extensions
    {
        /// <summary>
        /// Write the closure to the string builder.
        /// </summary>
        /// <param name="closure">The closure.</param>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        internal static void AsString(this Closure closure, bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append("closure: ");
                buf.Append(closure.ToString());
            }
        }
    }
}