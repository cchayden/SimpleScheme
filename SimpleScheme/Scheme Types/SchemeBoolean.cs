// <copyright file="SchemeBoolean.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Operations on boolean values.
    /// Booleans are immutable.
    /// </summary>
    public static class SchemeBoolean
    {
        #region Constants
        /// <summary>
        /// Define the true value.
        /// </summary>
        public const bool True = true;

        /// <summary>
        /// Define the false value.
        /// </summary>
        public const bool False = false;

        /// <summary>
        /// The printable name of the scheme boolean type.
        /// </summary>
        public const string Name = "boolean";
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Write the boolean to the string builder.
        /// </summary>
        /// <param name="value">The boolean value.</param>
        /// <param name="quoted">Whether to quote (not used).</param>
        /// <param name="buf">The string builder to write to.</param>
        public static void AsString(bool value, bool quoted, StringBuilder buf)
        {
            buf.Append(value ? "#t" : "#f");
        }

        /// <summary>
        /// Tests whether to given object is a scheme boolean.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme boolean.</returns>
        public static bool Is(Obj obj)
        {
            return obj is bool;
        }

        /// <summary>
        /// Check that an oject is a scheme boolean.
        /// </summary>
        /// <param name="obj">The object.</param>
        /// <returns>The scheme boolean.</returns>
        public static bool As(Obj obj)
        {
            if (Is(obj))
            {
                return (bool)obj;
            }

            ErrorHandlers.TypeError(Name, obj);
            return false;
        }

        /// <summary>
        /// Equality test for two objs.
        /// Two objs are equal if they:
        ///   are both empty lists
        ///   are both strings containing the same characters
        ///   are both vectors whose members are all equal, or
        ///   are equal by their type-specific Equals function.
        /// </summary>
        /// <param name="obj1">One member to test.</param>
        /// <param name="obj2">The other member to test.</param>
        /// <returns>True if the objs are equal.</returns>
        public static bool Equal(Obj obj1, Obj obj2)
        {
            // both empty list
            if (EmptyList.Is(obj1) || EmptyList.Is(obj2))
            {
                return obj1 == obj2;
            }

            // test strings
            if (SchemeString.Is(obj1))
            {
                return SchemeString.Equal(obj1, obj2);
            }

            // test vectors
            if (Vector.Is(obj1))
            {
                return Vector.Equal(obj1, obj2);
            }

            if (Pair.Is(obj1))
            {
                return Pair.Equal(obj1, obj2);
            }

            // delegate to first member, use C# equality
            return obj1.Equals(obj2);
        }

        /// <summary>
        /// Equivalence test.
        /// Two objs are equivalent if
        ///   they are equal as C# objects
        ///   they are equal booleans
        ///   they are equal numbers
        ///   they are equal character.
        /// </summary>
        /// <param name="obj1">The first obj.</param>
        /// <param name="obj2">The second obj.</param>
        /// <returns>True if they are equivalent.</returns>
        public static bool Eqv(Obj obj1, Obj obj2)
        {
            return obj1 == obj2 || 
                (obj1 is bool && obj1.Equals(obj2)) || 
                (obj1 is int && obj1.Equals(obj2)) || 
                (obj1 is double && obj1.Equals(obj2)) || 
                (obj1 is char && obj1.Equals(obj2));
        }

        /// <summary>
        /// Test an obj to see if it is false.
        /// If the obj is not a boolean, then it will not be false.
        /// </summary>
        /// <param name="value">The obj to test.</param>
        /// <returns>True if the value is a boolean and the boolean is false.</returns>
        public static bool IsFalse(Obj value)
        {
            return Is(value) && (bool)value == false;
        }

        /// <summary>
        /// Test an obj to see if it is true.
        /// If the obj is not a boolean, then it will not be true.
        /// </summary>
        /// <param name="value">The obj to test.</param>
        /// <returns>True if the value is a boolean and the boolean is true.</returns>
        public static bool IsTrue(Obj value)
        {
            return Is(value) && (bool)value;
        }

        /// <summary>
        /// Test to see if an obj is true.
        /// This is true if the obj is not a boolean, or if it is and is true.
        /// In the other scheme value classes, this method would be called Bool.
        /// </summary>
        /// <param name="obj">The obj to test.</param>
        /// <returns>True if a boolean and true, or else is not a boolean.</returns>
        public static bool Truth(Obj obj)
        {
            return !IsFalse(obj);
        }

        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the boolean primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(PrimitiveEnvironment env)
        {
            env
                //// <r4rs section="6.1">(boolean? <obj>)</r4rs>
                .DefinePrimitive("boolean?", (args, caller) => Truth(Is(List.First(args))), 1)
                //// <r4rs section="6.2">(eq? <obj1> <obj2>)</r4rs>
                .DefinePrimitive("eq?", (args, caller) => Truth(Eqv(List.First(args), List.Second(args))), 2)
                //// <r4rs section="6.2">(equal? <obj1> <obj2>)</r4rs>
                .DefinePrimitive("equal?", (args, caller) => Truth(Equal(List.First(args), List.Second(args))), 2)
                //// <r4rs section="6.2">(eqv? <obj1> <obj2>)</r4rs>
                .DefinePrimitive("eqv?", (args, caller) => Truth(Eqv(List.First(args), List.Second(args))), 2)
                //// <r4rs section="6.1">(not <obj>)</r4rs>
                .DefinePrimitive("not", (args, caller) => Truth(Is(List.First(args)) && (bool)List.First(args) == false), 1)
                //// <r4rs section="6.3">(null? <obj>)</r4rs>
                .DefinePrimitive("null?", (args, caller) => Truth(EmptyList.Is(List.First(args))), 1);
        }
        #endregion
    }
}
