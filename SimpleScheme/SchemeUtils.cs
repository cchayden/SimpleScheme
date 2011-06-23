﻿// <copyright file="SchemeUtils.cs" company="Charles Hayden">
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
    public class SchemeUtils
    {
        /// <summary>
        /// Define the true object.
        /// </summary>
        public static readonly bool True = true;

        /// <summary>
        /// Define the false object.
        /// </summary>
        public static readonly bool False = false;

        /// <summary>
        /// Define the zero object.
        /// </summary>
        public static readonly double Zero = 0.0D;

        /// <summary>
        /// Define the one object.
        /// </summary>
        public static readonly double One = 1.0D;

        /// <summary>
        /// Equality test for two objects.
        /// Two objects are equal if they:
        ///   are both null
        ///   are both strings containing the same characters
        ///   are both vectors whose members are all equal, or
        ///   are equal by their type-specific Equals function.
        /// </summary>
        /// <param name="x">One member to test.</param>
        /// <param name="y">The other member to test.</param>
        /// <returns>True if the objects are equal.</returns>
        public static bool Equal(object x, object y)
        {
            // both null
            if (x == null || y == null)
            {
                return x == y;
            }

            // test strings
            if (x is char[])
            {
                if (!(y is char[]))
                {
                    return false;
                }

                char[] xc = (char[])x;
                char[] yc = (char[])y;
                if (xc.Length != yc.Length)
                {
                    return false;
                }

                for (int i = xc.Length - 1; i >= 0; i--)
                {
                    if (xc[i] != yc[i])
                    {
                        return false;
                    }
                }

                return true;
            }

            // test vectors
            if (x is object[])
            {
                if (!(y is object[]))
                {
                    return false;
                }

                object[] xo = (object[])x;
                object[] yo = (object[])y;
                if (xo.Length != yo.Length)
                {
                    return false;
                }

                for (int i = xo.Length - 1; i >= 0; i--)
                {
                    if (!Equal(xo[i], yo[i]))
                    {
                        return false;
                    }
                }

                return true;
            }

            // delegate to first member, use C# equality
            return x.Equals(y);
        }

        /// <summary>
        /// Equivalence test.
        /// Two objects are equivalent if
        ///   they are equal as C# objects
        ///   they are equal booleans
        ///   they are equal numbers
        ///   they are equal character.
        /// </summary>
        /// <param name="x">The first object.</param>
        /// <param name="y">The second object.</param>
        /// <returns>True if they are equivalent.</returns>
        public static bool Eqv(object x, object y)
        {
            return x == y || 
                (x is bool && x.Equals(y)) || 
                (x is int && x.Equals(y)) || 
                (x is double && x.Equals(y)) || 
                (x is char && x.Equals(y));
        }

        // Errors and Warnings

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

        // List Utils

        /// <summary>
        /// Construct a pair from two objects.
        /// </summary>
        /// <param name="a">The first object.</param>
        /// <param name="b">The rest of the objects.</param>
        /// <returns>The pair resulting from the construction.</returns>
        public static Pair Cons(object a, object b)
        {
            return new Pair(a, b);
        }

        /// <summary>
        /// Determine the length of a list.
        /// </summary>
        /// <param name="x">The list to measure.</param>
        /// <returns>The list length.</returns>
        public static int Length(object x)
        {
            int len = 0;
            while (x is Pair)
            {
                len++;
                x = ((Pair)x).Rest;
            }

            return len;
        }

        /// <summary>
        /// Create a list from an object.
        /// Makes a list of length 1.
        /// </summary>
        /// <param name="a">The object to put into the list.</param>
        /// <returns>The Pair making up the head of the list.</returns>
        public static Pair List(object a)
        {
            return new Pair(a, null);
        }

        /// <summary>
        /// Create a list from two objects.
        /// Makes a list of length 2.
        /// </summary>
        /// <param name="a">The first object.</param>
        /// <param name="b">The second object.</param>
        /// <returns>The Pair making up the head of the list.</returns>
        public static Pair List(object a, object b)
        {
            return new Pair(a, new Pair(b, null));
        }

        /// <summary>
        /// Get the first member of a list.
        /// </summary>
        /// <param name="x">The list to use.</param>
        /// <returns>The first member of the list, or null if not a list.</returns>
        public static object First(object x)
        {
            return x is Pair ? ((Pair)x).First : null;
        }

        /// <summary>
        /// Get the second member of a list.
        /// </summary>
        /// <param name="x">The list to use.</param>
        /// <returns>The second member of the list, 
        /// or null if there is none.</returns>
        public static object Second(object x)
        {
            return First(Rest(x));
        }

        /// <summary>
        /// Get the third member of a list.
        /// </summary>
        /// <param name="x">The list to use.</param>
        /// <returns>The third member of the list, or null if there is none.</returns>
        public static object Third(object x)
        {
            return First(Rest(Rest(x)));
        }

        /// <summary>
        /// Return the rest of a list.
        /// </summary>
        /// <param name="x">The list to use.</param>
        /// <returns>The rest -- the list with the first stripped off, 
        /// or null if there is none.</returns>
        public static object Rest(object x)
        {
            return x is Pair ? ((Pair)x).Rest : null;
        }

        // Destructive list operations

        /// <summary>
        /// Set the first member of a pair destructively.
        /// </summary>
        /// <param name="x">The pair whose first member we want to modify.</param>
        /// <param name="y">The new value to put into it.</param>
        /// <returns>The object that has just been modified.</returns>
        public static object SetFirst(object x, object y)
        {
            return x is Pair ? 
                ((Pair)x).First = y : 
                Error("Attempt to set-car of a non-Pair: " + StringUtils.AsString(x));
        }

        /// <summary>
        /// Set the second member of a pair (the rest) destructively.
        /// </summary>
        /// <param name="x">The pair whose second member we want to modify.</param>
        /// <param name="y">The new value to put into it.</param>
        /// <returns>The object that has just been modified.</returns>
        public static object SetRest(object x, object y)
        {
            return x is Pair ? ((Pair)x).Rest = y : Error("Attempt to set-cdr of a non-Pair: " + StringUtils.AsString(x));
        }

        /// <summary>
        /// Create a list made out of all the objects given.
        /// </summary>
        /// <param name="args">The object to make into a list.</param>
        /// <returns>The items appended.</returns>
        public static object ListStar(object args)
        {
            return Rest(args) == null ? 
                First(args) : 
                Cons(First(args), ListStar(Rest(args)));
        }

        /// <summary>
        /// Create a list containing objects in the given list in the reverse order.
        /// </summary>
        /// <param name="x">The list to reverse.</param>
        /// <returns>The reversed list.</returns>
        public static object Reverse(object x)
        {
            object result = null;
            while (x is Pair)
            {
                result = Cons(First(x), result);
                x = Rest(x);
            }

            return result;
        }

        // String utilities

        /// <summary>
        /// Convert an object containing a character into the character.
        /// </summary>
        /// <param name="x">The object containing the char.</param>
        /// <returns>The character it contains.</returns>
        public static char Chr(object x)
        {
            if (!(x is char))
            {
                return Chr(Error("expected a char, got: " + x));
            }

            return (char)x;
        }

        /// <summary>
        /// Turn an object that is a symbol into a string.
        /// It is stored as one already, so just verify that this is a symbol.
        /// </summary>
        /// <param name="x">The symbol.</param>
        /// <returns>The corresponding string.</returns>
        public static string Sym(object x)
        {
            try
            {
                return (string)x;
            }
            catch (InvalidCastException)
            {
                return Sym(Error("expected a symbol, got: " + x));
            }
        }

        /// <summary>
        /// Test an object to see if it is false.
        /// If the object is not a boolean, then it will not be false.
        /// </summary>
        /// <param name="value">The object to test.</param>
        /// <returns>True if the value is a boolean and the boolean is false.</returns>
        public static bool IsFalse(object value)
        {
            bool? x = AsBoolean(value);
            return x.HasValue && x.Value == false;
        }

        /// <summary>
        /// Test an object to see if it is true.
        /// If the object is not a boolean, then it will not be true.
        /// </summary>
        /// <param name="value">The object to test.</param>
        /// <returns>True if the value is a boolean and the boolean is true.</returns>
        public static bool IsTrue(object value)
        {
            bool? x = AsBoolean(value);
            return x.HasValue && x.Value;
        }

        /// <summary>
        /// Test to see if an object is true.
        /// This is true if the object is not a boolean, or if it is and is true.
        /// </summary>
        /// <param name="x">The object to test.</param>
        /// <returns>True if a boolean and true, or else is not a boolean.</returns>
        public static bool Truth(object x)
        {
            return !IsFalse(x);
        }

        /// <summary>
        /// Turn an object that is a boolean into a bool.
        /// </summary>
        /// <param name="value">The object to convert.</param>
        /// <returns>The boolean value, or else null if not a boolean.</returns>
        private static bool? AsBoolean(object value)
        {
            if (!(value is bool))
            {
                return null;
            }

            return (bool)value;
        }

        /// <summary>
        /// All exceptions thrown by the interpreter are of this class.
        /// </summary>
        public class SchemeException : Exception
        {
            /// <summary>
            /// Initializes a new instance of the SchemeUtils.SchemeException class.
            /// </summary>
            /// <param name="message">The message describing the exception.</param>
            public SchemeException(string message) 
                : base(message)
            {
            }
        }
    }
}