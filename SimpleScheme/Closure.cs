// <copyright file="Closure.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// A closure stores the environment and a program to run.
    /// It can be executed later, through Apply.
    /// </summary>
    public class Closure : Procedure
    {
        /// <summary>
        /// Initializes a new instance of the Closure class.
        /// </summary>
        /// <param name="parms">A list of variable names, to be matched with 
        ///    values later.</param>
        /// <param name="body">The program to execute.</param>
        /// <param name="env">The environment in which to execute it.</param>
        public Closure(object parms, object body, Environment env)
        {
            this.Parms = parms;
            this.Env = env;
            this.Body = body is Pair && Rest(body) == null ? 
                First(body) :            // one expression
                Cons("begin", body);     // more than one expression --> (begin expressions)
        }

        /// <summary>
        /// Gets a list of variable names, to be matched with values later.
        /// </summary>
        public object Parms { get; private set; }

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
        /// <param name="interpreter">The interpreter, which has the global environment.</param>
        /// <param name="parent">The calling evaluator.</param>
        /// <param name="args">The values to be matched with the variable names.</param>
        /// <returns>The results of executing the program.</returns>
        public override object Apply(Scheme interpreter, Stepper parent, object args)
        {
            if (parent == null)
            {
                return interpreter.Eval(this.Body, new Environment(this.Parms, args, this.Env));
            }

            return Stepper.CallMain(interpreter, parent, this.Body, new Environment(this.Parms, args, this.Env));
        }
    }
}