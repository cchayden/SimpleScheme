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
        public static void PrintString(this bool value, bool quoted, StringBuilder buf)
        {
            buf.Append(value ? "#t" : "#f");
        }

        /// <summary>
        /// Tests whether a given object is a scheme boolean.
        /// </summary>
        /// <param name="obj">The object to test.</param>
        /// <returns>True if the object is a scheme boolean.</returns>
        public static bool IsSchemeBoolean(this Obj obj)
        {
            return obj is bool;
        }

        /// <summary>
        /// Check that an oject is a scheme boolean.
        /// </summary>
        /// <param name="obj">The object.</param>
        /// <returns>The scheme boolean.</returns>
        public static bool AsSchemeBoolean(this Obj obj)
        {
            if (obj.IsSchemeBoolean())
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
            if (obj1.IsEmptyList() && obj2.IsEmptyList())
            {
                return true;
            }

            if (obj1.IsSchemeString())
            {
                return SchemeString.Equal(obj1, obj2);
            }

            if (obj1.IsVector())
            {
                return Vector.Equal(obj1, obj2);
            }

            if (obj1.IsPair())
            {
                return Pair.Equal(obj1, obj2);
            }

            if (obj1.IsSymbol())
            {
                return Symbol.Equal(obj1, obj2);
            }

            if (obj1.IsNumber())
            {
                return Number.Normalize(obj1).Equals(Number.Normalize(obj2));
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
        ///   they are equal characters.
        /// </summary>
        /// <param name="obj1">The first obj.</param>
        /// <param name="obj2">The second obj.</param>
        /// <returns>True if they are equivalent.</returns>
        public static bool Eqv(Obj obj1, Obj obj2)
        {
            return obj1 == obj2 || 
                (obj1 is bool && obj1.Equals(obj2)) || 
                (obj1 != null && Number.Normalize(obj1).Equals(Number.Normalize(obj2))) ||
                (obj1 is char && obj1.Equals(obj2)) ||
                (obj1 != null && obj2 != null && obj1.IsSymbol() && obj2.IsSymbol() && obj1.ToString() == obj2.ToString());
        }

        /// <summary>
        /// Test an obj to see if it is false.
        /// If the obj is not a boolean, then it will not be false.
        /// </summary>
        /// <param name="value">The obj to test.</param>
        /// <returns>True if the value is a boolean and the boolean is false.</returns>
        public static bool IsFalse(Obj value)
        {
            return value.IsSchemeBoolean() && (bool)value == false;
        }

        /// <summary>
        /// Test an obj to see if it is true.
        /// If the obj is not a boolean, then it will not be true.
        /// </summary>
        /// <param name="value">The obj to test.</param>
        /// <returns>True if the value is a boolean and the boolean is true.</returns>
        public static bool IsTrue(Obj value)
        {
            return value.IsSchemeBoolean() && (bool)value;
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
                .DefinePrimitive(Symbol.New("boolean?"), (args, caller) => Truth(args.First().IsSchemeBoolean()), 1, Primitive.ValueType.Obj)
                //// <r4rs section="6.2">(eq? <obj1> <obj2>)</r4rs>
                .DefinePrimitive(Symbol.New("eq?"), (args, caller) => Truth(Eqv(args.First(), args.Second())), 2, Primitive.ValueType.Obj)
                //// <r4rs section="6.2">(equal? <obj1> <obj2>)</r4rs>
                .DefinePrimitive(Symbol.New("equal?"), (args, caller) => Truth(Equal(args.First(), args.Second())), 2, Primitive.ValueType.Obj)
                //// <r4rs section="6.2">(eqv? <obj1> <obj2>)</r4rs>
                .DefinePrimitive(Symbol.New("eqv?"), (args, caller) => Truth(Eqv(args.First(), args.Second())), 2, Primitive.ValueType.Obj)
                //// <r4rs section="6.1">(not <obj>)</r4rs>
                .DefinePrimitive(Symbol.New("not"), (args, caller) => Truth(args.First().IsSchemeBoolean() && (bool)args.First() == false), 1, Primitive.ValueType.Obj)
                //// <r4rs section="6.3">(null? <obj>)</r4rs>
                .DefinePrimitive(Symbol.New("null?"), (args, caller) => Truth(args.First().IsEmptyList()), 1, Primitive.ValueType.Obj);
        }
        #endregion
    }

    static class Extensions
    {
    }
}
