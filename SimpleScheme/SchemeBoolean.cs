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
    public sealed class SchemeBoolean : ListPrimitives
    {
        #region Constants
        /// <summary>
        /// Define the true value.
        /// </summary>
        internal const bool True = true;

        /// <summary>
        /// Define the false value.
        /// </summary>
        internal const bool False = false;
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Test an object's type.
        /// </summary>
        /// <param name="obj">The object to test.</param>
        /// <returns>True if the object is a scheme boolean.</returns>
        internal static bool IsType(Obj obj)
        {
            return obj is bool;
        }

        /// <summary>
        /// Give the name of the type (for display).
        /// </summary>
        /// <returns>The type name.</returns>
        internal static string TypeName()
        {
            return "bool";
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
        internal static bool Equal(Obj obj1, Obj obj2)
        {
            // both empty list
            if (obj1 == List.Empty || obj2 == List.Empty)
            {
                return obj1 == obj2;
            }

            // test strings
            if (obj1 is char[])
            {
                return SchemeString.Equal(obj1, obj2);
            }

            // test vectors
            if (obj1 is Obj[])
            {
                return Vector.Equal(obj1, obj2);
            }

            if (obj1 is Pair)
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
        internal static bool Eqv(Obj obj1, Obj obj2)
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
        internal static bool IsFalse(Obj value)
        {
            return (value is bool) && ((bool)value) == false;
        }

        /// <summary>
        /// Test an obj to see if it is true.
        /// If the obj is not a boolean, then it will not be true.
        /// </summary>
        /// <param name="value">The obj to test.</param>
        /// <returns>True if the value is a boolean and the boolean is true.</returns>
        internal static bool IsTrue(Obj value)
        {
            return (value is bool) && (bool)value;
        }

        /// <summary>
        /// Test to see if an obj is true.
        /// This is true if the obj is not a boolean, or if it is and is true.
        /// </summary>
        /// <param name="obj">The obj to test.</param>
        /// <returns>True if a boolean and true, or else is not a boolean.</returns>
        internal static bool Truth(Obj obj)
        {
            return !IsFalse(obj);
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the boolean primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        internal static void DefinePrimitives(Environment env)
        {
            env
                //// <r4rs section="6.1">(boolean? <obj>)</r4rs>
                .DefinePrimitive("boolean?", (args, caller) => Truth(First(args) is bool), 1)
                //// <r4rs section="6.2">(eq? <obj1> <obj2>)</r4rs>
                .DefinePrimitive("eq?", (args, caller) => Truth(Eqv(First(args), Second(args))), 2)
                //// <r4rs section="6.2">(equal? <obj1> <obj2>)</r4rs>
                .DefinePrimitive("equal?", (args, caller) => Truth(Equal(First(args), Second(args))), 2)
                //// <r4rs section="6.2">(eqv? <obj1> <obj2>)</r4rs>
                .DefinePrimitive("eqv?", (args, caller) => Truth(Eqv(First(args), Second(args))), 2)
                //// <r4rs section="6.1">(not <obj>)</r4rs>
                .DefinePrimitive("not", (args, caller) => Truth(First(args) is bool && (bool)First(args) == false), 1)
                //// <r4rs section="6.3">(null? <obj>)</r4rs>
                .DefinePrimitive("null?", (args, caller) => Truth(First(args) == List.Empty), 1);
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Convert the bool instance to a string.
        /// </summary>
        /// <param name="val">The boolean value to convert.</param>
        /// <param name="quoted">True if the string should be quoted.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        internal static void AsString(bool val, bool quoted, StringBuilder buf)
        {
            buf.Append(val ? "#t" : "#f");
        }
        #endregion
    }
}
