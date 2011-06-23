// <copyright file="SchemeBoolean.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Operations on boolean values.
    /// </summary>
    public sealed class SchemeBoolean : ListPrimitives
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
                .DefinePrimitive("boolean?", (caller, args) => Truth(First(args) is bool), 1)
                //// <r4rs section="6.2">(eq? <obj1> <obj2>)</r4rs>
                .DefinePrimitive("eq?", (caller, args) => Truth(Eqv(First(args), Second(args))), 2)
                //// <r4rs section="6.2">(equal? <obj1> <obj2>)</r4rs>
                .DefinePrimitive("equal?", (caller, args) => Truth(Equal(First(args), Second(args))), 2)
                //// <r4rs section="6.2">(eqv? <obj1> <obj2>)</r4rs>
                .DefinePrimitive("eqv?", (caller, args) => Truth(Eqv(First(args), Second(args))), 2)
                //// <r4rs section="6.1">(not <obj>)</r4rs>
                .DefinePrimitive("not", (caller, args) => Truth(First(args) is bool && (bool)First(args) == false), 1)
                //// <r4rs section="6.3">(null? <obj>)</r4rs>
                .DefinePrimitive("null?", (caller, args) => Truth(First(args) == null), 1);
        }

        /// <summary>
        /// Equality test for two objects.
        /// Two objects are equal if they:
        ///   are both null
        ///   are both strings containing the same characters
        ///   are both vectors whose members are all equal, or
        ///   are equal by their type-specific Equals function.
        /// </summary>
        /// <param name="obj1">One member to test.</param>
        /// <param name="obj2">The other member to test.</param>
        /// <returns>True if the objects are equal.</returns>
        public static bool Equal(object obj1, object obj2)
        {
            // both null
            if (obj1 == null || obj2 == null)
            {
                return obj1 == obj2;
            }

            // test strings
            if (obj1 is SchemeString)
            {
                if (!(obj2 is SchemeString))
                {
                    return false;
                }

                return SchemeString.Equal((SchemeString)obj1, (SchemeString)obj2);
            }

            // test vectors
            if (obj1 is object[])
            {
                if (!(obj2 is object[]))
                {
                    return false;
                }

                return Vector.Equal((object[])obj1, (object[])obj2);
            }

            // delegate to first member, use C# equality
            return obj1.Equals(obj2);
        }

        /// <summary>
        /// Equivalence test.
        /// Two objects are equivalent if
        ///   they are equal as C# objects
        ///   they are equal booleans
        ///   they are equal numbers
        ///   they are equal character.
        /// </summary>
        /// <param name="obj1">The first object.</param>
        /// <param name="obj2">The second object.</param>
        /// <returns>True if they are equivalent.</returns>
        public static bool Eqv(object obj1, object obj2)
        {
            return obj1 == obj2 || 
                (obj1 is bool && obj1.Equals(obj2)) || 
                (obj1 is int && obj1.Equals(obj2)) || 
                (obj1 is double && obj1.Equals(obj2)) || 
                (obj1 is char && obj1.Equals(obj2));
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
        /// <param name="obj">The object to test.</param>
        /// <returns>True if a boolean and true, or else is not a boolean.</returns>
        public static bool Truth(object obj)
        {
            return !IsFalse(obj);
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
