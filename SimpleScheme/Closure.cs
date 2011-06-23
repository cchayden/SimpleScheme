// <copyright file="Closure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// A closure stores the environment and a program to run.
    /// It can be executed later, through Apply.
    /// The Macro class also derives from this.
    /// </summary>
    public class Closure : Procedure
    {
        /// <summary>
        /// Initializes a new instance of the Closure class.
        /// </summary>
        /// <param name="formalParameters">A list of variable names, to be matched with 
        ///    values given later.</param>
        /// <param name="body">The program to execute.</param>
        /// <param name="env">The environment in which to execute it.</param>
        public Closure(object formalParameters, object body, Environment env)
        {
            this.FormalParameters = formalParameters;
            this.Env = env;
            this.Body = body is Pair && List.Rest(body) == null ? 
                List.First(body) :            // one expression
                List.Cons("begin", body);     // more than one expression --> (begin expressions)
        }

        /// <summary>
        /// Gets a list of variable names, to be matched with values later.
        /// </summary>
        public object FormalParameters { get; private set; }

        /// <summary>
        /// Gets the program to execute.
        /// </summary>
        public object Body { get; private set; }

        /// <summary>
        /// Gets the environment in which to execute.
        /// </summary>
        public Environment Env { get; private set; }

        /// <summary>
        /// Actually executes the saved program, with the given arguments matched with the 
        ///   list of variable names saved when the continuation was created.
        /// </summary>
        /// <param name="parent">The calling evaluator.</param>
        /// <param name="args">The values to be matched with the variable names.</param>
        /// <returns>The results of executing the program.</returns>
        public override object Apply(Stepper parent, object args)
        {
            return EvaluatorMain.New(this.Body, 
                new Environment(this.FormalParameters, args, this.Env), parent);
        }

        /// <summary>
        /// Display the closure as a string.  
        /// Displays the formal parameters and the body, as it has been processed by the reader.
        /// </summary>
        /// <returns>The string form of the closure.</returns>
        public override string ToString()
        {
            return string.Format("(lambda {0} {1})", this.FormalParameters, this.Body);
        }
    }
}