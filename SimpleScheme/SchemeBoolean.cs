// <copyright file="SchemeBoolean.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Operations on boolean values.
    /// </summary>
    public sealed class SchemeBoolean
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
        /// Define the boolean primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(Environment env)
        {
            env
                //// <r4rs section="6.1">(boolean? <obj>)</r4rs>
                .DefinePrimitive("boolean?", (parent, args) => Truth(List.First(args) is bool), 1)
                //// <r4rs section="6.2">(eq? <obj1> <obj2>)</r4rs>
                .DefinePrimitive("eq?", (parent, args) => Truth(Eqv(List.First(args), List.Second(args))), 2)
                //// <r4rs section="6.2">(equal? <obj1> <obj2>)</r4rs>
                .DefinePrimitive("equal?", (parent, args) => Truth(Equal(List.First(args), List.Second(args))), 2)
                //// <r4rs section="6.2">(eqv? <obj1> <obj2>)</r4rs>
                .DefinePrimitive("eqv?", (parent, args) => Truth(Eqv(List.First(args), List.Second(args))), 2)
                //// <r4rs section="6.1">(not <obj>)</r4rs>
                .DefinePrimitive("not", (parent, args) => Truth(List.First(args) is bool && (bool)List.First(args) == false), 1)
                //// <r4rs section="6.3">(null? <obj>)</r4rs>
                .DefinePrimitive("null?", (parent, args) => Truth(List.First(args) == null), 1);
        }

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
            if (x is SchemeString)
            {
                if (!(y is SchemeString))
                {
                    return false;
                }

                return SchemeString.Equal((SchemeString)x, (SchemeString)y);
            }

            // test vectors
            if (x is Vector)
            {
                if (!(y is Vector))
                {
                    return false;
                }

                return Vector.Equal((Vector)x, (Vector)y);
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
    }
}
