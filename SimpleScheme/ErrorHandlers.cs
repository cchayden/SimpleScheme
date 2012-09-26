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
        public static void DefinePrimitives(PrimitiveEnvironment env)
        {
            const int MaxInt = int.MaxValue;
            env
                .DefinePrimitive(
                    "error", 
                    new[] { "(error <message> ...)" }, 
                    (args, caller) => Error(args.ToString(true)), 
                    0, 
                    MaxInt, 
                    Primitive.ArgType.String);
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
        public static SchemeObject Error(string message)
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
        public static SchemeObject TypeError(Type expected, EvaluatorOrObject got)
        {
            string message = string.Format(
                "Invalid type: expected {0}, got {1}: {2}", 
                expected.SchemeTypeName(), 
                got.SchemeTypeName(), 
                got.ToString(true));
            Console.Error.WriteLine("**** TYPE ERROR: {0}", message);
            throw new SchemeTypeException(message);
        }

        /// <summary>
        /// Handle an error by printing a message on the console 
        ///    and throwing an exception.
        /// These errors are caused by a failed attempt to print a given value.
        /// All SchemeObject objects implement PrintString.
        /// </summary>
        /// <param name="got">The actual value.</param>
        /// <returns>Actually, does not return.</returns>
        public static SchemeObject PrintError(SchemeObject got)
        {
            string message = string.Format(
                "Invalid type whem printing: expected SchemeObject, got {0}: {1}", 
                got.SchemeTypeName(), 
                got);
            Console.Error.WriteLine("**** PRINT ERROR: {0}", message);
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
        public static SchemeObject IoError(string message)
        {
            Console.Error.WriteLine("**** I/O ERROR: {0}", message);
            throw new SchemeIoException(message);
        }

        /// <summary>
        /// Handle an error by printing a message on the console 
        ///    and throwing an exception.
        /// These are public errors, and should never happen.
        /// </summary>
        /// <param name="message">The message to display and to put 
        ///    into the exception.</param>
        /// <returns>Actually, does not return.</returns>
        public static SchemeObject InternalError(string message)
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
        public static SchemeObject ClrError(string message)
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
        public static SchemeObject SemanticError(string message)
        {
            Console.Error.WriteLine("**** SEMANTIC ERROR: {0}", message);
            throw new SchemeSemanticException(message);
        }

        /// <summary>
        /// Handle an error by printing a message on the console 
        ///    and throwing an exception.
        /// This is a semantic error, caused by trying to Apply a value that is not a procedure.
        /// </summary>
        /// <param name="message">The message to display and to put 
        ///    into the exception.</param>
        /// <param name="got">The object we got instead of he proc.</param>
        /// <returns>Actually, does not return.</returns>
        public static SchemeObject ProcError(string message, SchemeObject got)
        {
            string msg = string.Format("{0}, got {1}: {2}", message, got.SchemeTypeName(), got.ToString(true));
            Console.Error.WriteLine("**** PROCEDURE ERROR: {0}", msg);
            throw new SchemeProcException(message);
        }

        /// <summary>
        /// Handle an error by printing a message on the console 
        ///    and throwing an exception.
        /// This is an invalid operation.
        /// </summary>
        /// <param name="message">The message to display and to put 
        ///    into the exception.</param>
        /// <returns>Actually, does not return.</returns>
        public static SchemeObject InvalidOperationError(string message)
        {
            Console.Error.WriteLine("**** INVALID OPERATION ERROR: {0}", message);
            throw new InvalidOperationException(message);
        }

        /// <summary>
        /// Print exception.
        /// </summary>
        /// <param name="ex">The exception to print.</param>
        public static void PrintException(Exception ex)
        {
            // If this is a scheme exception, it has already been reported.
            if (!(ex is SchemeException))
            {
                Console.WriteLine("Caught exception {0}", ex.Message);
            }
        }
        #endregion

        #region Exception Class
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
        #endregion

        #region Exception Subclasses
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
            public SchemeTypeException(string message) 
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
            public SchemeIoException(string message) 
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
            public SchemeInternalException(string message) 
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
            public SchemeSemanticException(string message) 
                : base(message)
            {
            }
        }

        /// <summary>
        /// All exceptions thrown by the interpreter are of this class.
        /// </summary>
        [Serializable]
        public class SchemeProcException : SchemeException
        {
            /// <summary>
            /// Initializes a new instance of the ErrorHandlers.SchemeProcException class.
            /// </summary>
            /// <param name="message">The message describing the exception.</param>
            public SchemeProcException(string message) 
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
            public SchemeClrException(string message) 
                : base(message)
            {
            }
        }
        #endregion
    }
}