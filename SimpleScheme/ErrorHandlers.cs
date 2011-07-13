// <copyright file="ErrorHandlers.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using Obj = System.Object;

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
        #region Public Static Methods
        /// <summary>
        /// Display a warning and return a warning string.
        /// </summary>
        /// <param name="message">The message to display.</param>
        public static void Warn(string message)
        {
            Console.Error.WriteLine("**** WARNING: " + message);
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the error primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        internal static void DefinePrimitives(Environment env)
        {
            const int MaxInt = int.MaxValue;
            env
                //// (error <message> ...)
                .DefinePrimitive("error", (args, caller) => Error(SchemeString.AsString(args)), 0, MaxInt);
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Handle an error by printing a message on the console 
        ///    and throwing an exception.
        /// </summary>
        /// <param name="message">The message to display and to put 
        ///    into the exception.</param>
        /// <returns>Actually, does not return.</returns>
        internal static object Error(string message)
        {
            Console.Error.WriteLine("**** ERROR: " + message);
            throw new SchemeException(message);
        }

        /// <summary>
        /// Handle an error by printing a message on the console 
        ///    and throwing an exception.
        /// These errors are caused by a failed attempt to interpret an object as a value
        ///   of a specific type.
        /// </summary>
        /// <param name="expected">The expected type.</param>
        /// <param name="got">The actual value.</param>
        /// <returns>Actually, does not return.</returns>
        internal static object TypeError(string expected, object got)
        {
            string message = String.Format("Invalid type: expected a {0}, got {1}: {2}", expected, TypePrimitives.TypeName(got), SchemeString.AsString(got));
            Console.Error.WriteLine("**** TYPE ERROR: {0}", message);
            throw new SchemeTypeException(message);
        }

        /// <summary>
        /// Handle an error by printing a message on the console 
        ///    and throwing an exception.
        /// These errors represent input/output errors.
        /// </summary>
        /// <param name="message">The message to display and to put 
        ///    into the exception.</param>
        /// <returns>Actually, does not return.</returns>
        internal static object IoError(string message)
        {
            Console.Error.WriteLine("**** I/O ERROR: {0}", message);
            throw new SchemeIoException(message);
        }

        /// <summary>
        /// Handle an error by printing a message on the console 
        ///    and throwing an exception.
        /// These are internal errors, and should never happen.
        /// </summary>
        /// <param name="message">The message to display and to put 
        ///    into the exception.</param>
        /// <returns>Actually, does not return.</returns>
        internal static object InternalError(string message)
        {
            Console.Error.WriteLine("**** INTERNAL ERROR: {0}", message);
            throw new SchemeInternalException(message);
        }

        /// <summary>
        /// Handle an error by printing a message on the console 
        ///    and throwing an exception.
        /// This is a problem with linking up with the CLR.
        /// </summary>
        /// <param name="message">The message to display and to put 
        ///    into the exception.</param>
        /// <returns>Actually, does not return.</returns>
        internal static object ClrError(string message)
        {
            Console.Error.WriteLine("**** CLR ERROR: {0}", message);
            throw new SchemeClrException(message);
        }

        /// <summary>
        /// Handle an error by printing a message on the console 
        ///    and throwing an exception.
        /// This is a semantic error, such as giving the wrong number of arguments to a primitive.
        /// </summary>
        /// <param name="message">The message to display and to put 
        ///    into the exception.</param>
        /// <returns>Actually, does not return.</returns>
        internal static object SemanticError(string message)
        {
            Console.Error.WriteLine("**** SEMANTIC ERROR: {0}", message);
            throw new SchemeSemanticException(message);
        }
        #endregion

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
            internal SchemeException(string message) 
                : base(message)
            {
            }
        }

        /// <summary>
        /// All exceptions thrown by the interpreter are of this class.
        /// </summary>
        [Serializable]
        public class SchemeTypeException : SchemeException
        {
            /// <summary>
            /// Initializes a new instance of the ErrorHandlers.SchemeTypeException class.
            /// This is thrown when there is an expectation that a value will be a certain type,
            /// and it is some other type.
            /// </summary>
            /// <param name="message">The message describing the exception.</param>
            internal SchemeTypeException(string message) 
                : base(message)
            {
            }
        }

        /// <summary>
        /// All exceptions thrown by the interpreter are of this class.
        /// </summary>
        [Serializable]
        public class SchemeIoException : SchemeException
        {
            /// <summary>
            /// Initializes a new instance of the ErrorHandlers.SchemeIoException class.
            /// </summary>
            /// <param name="message">The message describing the exception.</param>
            internal SchemeIoException(string message) 
                : base(message)
            {
            }
        }

        /// <summary>
        /// All exceptions thrown by the interpreter are of this class.
        /// </summary>
        [Serializable]
        public class SchemeInternalException : SchemeException
        {
            /// <summary>
            /// Initializes a new instance of the ErrorHandlers.SchemeInternalException class.
            /// </summary>
            /// <param name="message">The message describing the exception.</param>
            internal SchemeInternalException(string message) 
                : base(message)
            {
            }
        }

        /// <summary>
        /// All exceptions thrown by the interpreter are of this class.
        /// </summary>
        [Serializable]
        public class SchemeSemanticException : SchemeException
        {
            /// <summary>
            /// Initializes a new instance of the ErrorHandlers.SchemeSemanticException class.
            /// </summary>
            /// <param name="message">The message describing the exception.</param>
            internal SchemeSemanticException(string message) 
                : base(message)
            {
            }
        }

        /// <summary>
        /// All exceptions thrown by the interpreter are of this class.
        /// </summary>
        [Serializable]
        public class SchemeClrException : SchemeException
        {
            /// <summary>
            /// Initializes a new instance of the ErrorHandlers.SchemeClrException class.
            /// </summary>
            /// <param name="message">The message describing the exception.</param>
            internal SchemeClrException(string message) 
                : base(message)
            {
            }
        }
    }
}