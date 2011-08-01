// <copyright file="SchemeBoolean.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Operations on boolean values.
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
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Tests whether to given object is a scheme boolean.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme boolean.</returns>
        public static bool IsBoolean(Obj obj)
        {
            return obj is bool;
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
            if (EmptyList.IsEmptyList(obj1) || EmptyList.IsEmptyList(obj2))
            {
                return obj1 == obj2;
            }

            // test strings
            if (SchemeString.IsString(obj1))
            {
                return SchemeString.Equal(obj1, obj2);
            }

            // test vectors
            if (Vector.IsVector(obj1))
            {
                return Vector.Equal(obj1, obj2);
            }

            if (Pair.IsPair(obj1))
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
            return IsBoolean(value) && (bool)value == false;
        }

        /// <summary>
        /// Test an obj to see if it is true.
        /// If the obj is not a boolean, then it will not be true.
        /// </summary>
        /// <param name="value">The obj to test.</param>
        /// <returns>True if the value is a boolean and the boolean is true.</returns>
        public static bool IsTrue(Obj value)
        {
            return IsBoolean(value) && (bool)value;
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
                .DefinePrimitive("boolean?", (args, caller) => Truth(IsBoolean(List.First(args))), 1)
                //// <r4rs section="6.2">(eq? <obj1> <obj2>)</r4rs>
                .DefinePrimitive("eq?", (args, caller) => Truth(Eqv(List.First(args), List.Second(args))), 2)
                //// <r4rs section="6.2">(equal? <obj1> <obj2>)</r4rs>
                .DefinePrimitive("equal?", (args, caller) => Truth(Equal(List.First(args), List.Second(args))), 2)
                //// <r4rs section="6.2">(eqv? <obj1> <obj2>)</r4rs>
                .DefinePrimitive("eqv?", (args, caller) => Truth(Eqv(List.First(args), List.Second(args))), 2)
                //// <r4rs section="6.1">(not <obj>)</r4rs>
                .DefinePrimitive("not", (args, caller) => Truth(IsBoolean(List.First(args)) && (bool)List.First(args) == false), 1)
                //// <r4rs section="6.3">(null? <obj>)</r4rs>
                .DefinePrimitive("null?", (args, caller) => Truth(EmptyList.IsEmptyList(List.First(args))), 1);
        }
        #endregion
    }

    #region Extensions
    /// <summary>
    /// Provide common operations as extensions.
    /// </summary>
    public static partial class Extensions
    {
        /// <summary>
        /// Write the boolean to the string builder.
        /// </summary>
        /// <param name="value">The boolean value.</param>
        /// <param name="quoted">Whether to quote (not used).</param>
        /// <param name="buf">The string builder to write to.</param>
        public static void AsString(this bool value, bool quoted, StringBuilder buf)
        {
            buf.Append(value ? "#t" : "#f");
        }
    }
    #endregion
}
