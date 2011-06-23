// <copyright file="ErrorHandlers.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// Utilities for manipulating lists and vectors, input and output, and converting 
    ///    strings, numbers, characters, vectors, and booleans to native form.
    /// This has no member variables and contains only static methods.
    /// Others inhert from it only to get these static utility functions without having to 
    ///    prefix them with a class name.
    /// </summary>
    public sealed class ErrorHandlers
    {
        // Errors and Warnings

        /// <summary>
        /// Define the error primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(Environment env)
        {
            const int MaxInt = int.MaxValue;
            env
                .DefinePrimitive("error", (parent, args) => Error(SchemeString.AsString(args)), 0, MaxInt);
        }

        /// <summary>
        /// Handle an error by printing a message on the console 
        ///    and throwing an exception.
        /// </summary>
        /// <param name="message">The message to display and to put 
        ///    into the exception.</param>
        /// <returns>Actually, does not return.</returns>
        public static object Error(string message)
        {
            Console.Error.WriteLine("**** ERROR: " + message);
            throw new SchemeException(message);
        }

        /// <summary>
        /// Handle an error in an evaluator by printing a message on the console 
        ///    and throwing an exception.
        /// </summary>
        /// <param name="message">The message to display and to put 
        ///    into the exception.</param>
        /// <returns>Actually, does not return.</returns>
        public static Stepper EvalError(string message)
        {
            Console.Error.WriteLine("**** ERROR: " + message);
            throw new SchemeException(message);
        }

        /// <summary>
        /// Display a warning and return a warning string.
        /// </summary>
        /// <param name="message">The message to display.</param>
        /// <returns>A string warning, which does not contain the message.</returns>
        public static object Warn(string message)
        {
            Console.Error.WriteLine("**** WARNING: " + message);
            return "<warn>";
        }

        /// <summary>
        /// All exceptions thrown by the interpreter are of this class.
        /// </summary>
        [Serializable]
        public class SchemeException : Exception
        {
            /// <summary>
            /// Initializes a new instance of the ErrorHandlers.SchemeException class.
            /// </summary>
            /// <param name="message">The message describing the exception.</param>
            public SchemeException(string message) 
                : base(message)
            {
            }
        }
    }
}