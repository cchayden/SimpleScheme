#define OLD
// <copyright file="Continuation.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// Represents a continuation.
    /// This only works in a restricted sense, since it is implemented by throwing and 
    ///    catching an exception.
    /// </summary>
    public class Continuation : Procedure
    {
#if OLD
        /// <summary>
        /// The exception to throw.
        /// </summary>
        private readonly Exception cc;
#else
        private readonly Evaluator step;
#endif

#if OLD
        /// <summary>
        /// Initializes a new instance of the Continuation class.
        /// </summary>
        /// <param name="cc">The exception to throw.</param>
        public Continuation(Exception cc)
        {
            this.cc = cc;
        }
#else

        public Continuation(Evaluator step)
        {
            this.step = step;
        }
#endif

        /// <summary>
        /// Gets the value to return as the result of executing the continuation.
        /// </summary>
        public object Value { get; private set; }

        /// <summary>
        /// Execute the continuation.
        /// </summary>
        /// <param name="interpreter">The interpreter to execute in.</param>
        /// <param name="parent">The calling evaluator.</param>
        /// <param name="args">The value to return.</param>
        /// <returns>The result of applying the continuation.</returns>
        public override object Apply(Scheme interpreter, Evaluator parent, object args)
        {
            this.Value = First(args);
#if OLD
            throw this.cc;
#else
            return parent;
#endif
        }
    }
}

// TODO need to define an Evaluator that captures the Value in the RetExpr.
// Then remember this evaluator, and have Apply return it.