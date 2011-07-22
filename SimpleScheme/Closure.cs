// <copyright file="Closure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
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
            return TypePrimitives.IsEmptyList(List.Rest(this.Body)) ? 
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

            return string.Format("({0} {1} {2})", tag, this.FormalParameters, body);
        }
        #endregion
    }
}