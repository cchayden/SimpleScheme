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
    public class Closure : Procedure
    {
        /// <summary>
        /// Initializes a new instance of the Closure class.
        /// </summary>
        /// <param name="formalParameters">A list of variable names, to be matched with 
        ///    values given later.</param>
        /// <param name="body">The program to execute.</param>
        /// <param name="env">The environment in which to execute it.</param>
        public Closure(Obj formalParameters, Obj body, Environment env)
        {
            this.FormalParameters = formalParameters;
            this.Env = env;
            this.Body = body;
        }

        /// <summary>
        /// Gets a list of variable names, to be matched with values later.
        /// </summary>
        public Obj FormalParameters { get; private set; }

        /// <summary>
        /// Gets the program to execute.
        /// </summary>
        public Obj Body { get; private set; }

        /// <summary>
        /// Gets the environment in which to execute.
        /// </summary>
        public Environment Env { get; private set; }

        /// <summary>
        /// Actually executes the saved program, with the given arguments matched with the 
        ///   list of variable names saved when the closure was created.
        /// </summary>
        /// <param name="args">The values to be matched with the variable names.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next step to execute.</returns>
        public override Stepper Apply(Obj args, Stepper caller)
        {
            return EvaluateSequence.Call(this.Body, new Environment(this.FormalParameters, args, this.Env), caller);
        }

        /// <summary>
        /// Actually executes the saved program, with the given arguments matched with the 
        ///   list of variable names saved when the closure was created.
        /// Uses environment of the caller rather than creating a new one.
        /// </summary>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next step to execute.</returns>
        public Stepper ApplyWithCurrentEnv(Stepper caller)
        {
            return EvaluateSequence.Call(this.Body, caller.Env, caller);
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
            string formals = this.FormalParameters == List.Empty ? "()" : this.FormalParameters.ToString();
            string body = this.Body == List.Empty ? "()" : this.Body.ToString();
            return string.Format("({0} {1} {2})", tag, formals, body);
        }
    }
}