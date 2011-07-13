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
        protected Closure(Obj formalParameters, Obj body, Environment env)
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

        #region Internal Static Methods
        /// <summary>
        /// Test an object's type.
        /// </summary>
        /// <param name="obj">The object to test.</param>
        /// <returns>True if the object is a scheme macroclosure.</returns>
        internal static new bool IsType(Obj obj)
        {
            return obj is Closure;
        }

        /// <summary>
        /// Give the name of the type (for display).
        /// </summary>
        /// <returns>The type name.</returns>
        internal static new string TypeName()
        {
            return "closure";
        }

        /// <summary>
        /// Create a new Closure
        /// </summary>
        /// <param name="formalParameters">A list of variable names, to be matched with 
        ///    values given later.</param>
        /// <param name="body">The program to execute.</param>
        /// <param name="env">The environment in which to execute it.</param>
        /// <returns>An instance of the Closure class.</returns>
        internal static Closure New(Obj formalParameters, Obj body, Environment env)
        {
            return new Closure(formalParameters, body, env);
        }

        /// <summary>
        /// Actually executes the saved program, with the given arguments matched with the 
        ///   list of variable names saved when the closure was created.
        /// Creates a new environment, linked to the one given (which should be the lexical parent).
        /// </summary>
        /// <param name="args">The values to be matched with the variable names.</param>
        /// <param name="env">The base environment to evaluate in.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next step to execute.</returns>
        internal override Stepper Apply(Obj args, Environment env, Stepper caller)
        {
            return EvaluateSequence.Call(this.Body, Environment.New(this.FormalParameters, args, this.Env), caller);
        }

        /// <summary>
        /// Actually executes the saved program, with the given arguments matched with the 
        ///   list of variable names saved when the closure was created.
        /// Uses the given environment rather than creating a new one.
        /// </summary>
        /// <param name="env">The environment to evaluate in.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next step to execute.</returns>
        internal Stepper ApplyWithtEnv(Environment env, Stepper caller)
        {
            return EvaluateSequence.Call(this.Body, env, caller);
        }

        /// <summary>
        /// Display the closure as a string.  
        /// Displays the formal parameters and the body, as it has been processed by the reader.
        /// </summary>
        /// <returns>The string form of the closure.</returns>
        public override string ToString()
        {
            return this.ToString("lambda");
        }

        /// <summary>
        /// Common printing logic for closures.
        /// </summary>
        /// <param name="tag">The function name.</param>
        /// <returns>String representing the closure.</returns>
        protected string ToString(string tag)
        {
            string formals = EmptyList.IsType(this.FormalParameters) ? "()" : this.FormalParameters.ToString();
            string body = EmptyList.IsType(this.Body) ? "()" : this.Body.ToString();
            return string.Format("({0} {1} {2})", tag, formals, body);
        }
        #endregion
    }
}