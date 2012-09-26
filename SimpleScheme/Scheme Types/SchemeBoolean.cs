// <copyright file="SchemeBoolean.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;
    using System.Text;

    /// <summary>
    /// Operations on boolean values.
    /// Booleans are immutable.
    /// </summary>
    public class SchemeBoolean : SchemeObject
    {
        #region Static Fields
        /// <summary>
        /// Define the true value.
        /// </summary>
        public static readonly SchemeBoolean True = new SchemeBoolean(true);

        /// <summary>
        /// Define the false value.
        /// </summary>
        public static readonly SchemeBoolean False = new SchemeBoolean(false);

        /// <summary>
        /// This is used to do equality tests for different types.
        /// </summary>
        private static readonly Dictionary<Type, Func<SchemeObject, SchemeObject, SchemeBoolean>> equalMap;
        #endregion

        #region Fields
        /// <summary>
        /// The boolean value.
        /// </summary>
        private readonly bool value;
        #endregion

        #region Constructors
        /// <summary>
        /// Initializes static members of the <see cref="SchemeBoolean"/> class.
        /// </summary>
        static SchemeBoolean()
        {
            equalMap = new Dictionary<Type, Func<SchemeObject, SchemeObject, SchemeBoolean>>
                {
                    { typeof(EmptyList), (obj1, obj2) => obj2 is EmptyList ? True : False },
                    { typeof(SchemeString), (obj1, obj2) => ((SchemeString)obj1).Equals(obj2) ? True : False },
                    { typeof(Character), (obj1, obj2) => ((Character)obj1).Equals(obj2) ? True : False },
                    { typeof(Vector), (obj1, obj2) => Vector.Equal((Vector)obj1, obj2) },
                    { typeof(Pair), (obj1, obj2) => Pair.Equal((Pair)obj1, obj2) },
                    { typeof(Symbol), (obj1, obj2) => ((Symbol)obj1).Equals(obj2) ? True : False },
                    { typeof(SchemeBoolean), (obj1, obj2) => ((SchemeBoolean)obj1).Equals(obj2) ? True : False },
                    { typeof(Number), (obj1, obj2) => ((Number)obj1).Equals(obj2) ? True : False },
                };
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="SchemeBoolean"/> class.
        /// </summary>
        /// <param name="value">The boolean value.</param>
        private SchemeBoolean(bool value)
        {
            this.value = value;
        }
        #endregion

        #region SchemeType Accessors
        /// <summary>
        /// Gets the name of the type.
        /// </summary>
        public override string TypeName
        {
            get { return ValueTypeName(ValueType.Boolean); }
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets a value indicating whether the boolean value is true.
        /// </summary>
        public bool Value
        {
            get { return this.value; }
        }
        #endregion

        #region New
        /// <summary>
        /// Converts a bool into a SchemeBoolean.
        /// </summary>
        /// <param name="b">The bool.</param>
        /// <returns>The corresponding Schemeboolean.</returns>
        public static implicit operator SchemeBoolean(bool b)
        {
            return New(b);
        }

        /// <summary>
        /// Convert a boolean into a scheme boolean.
        /// </summary>
        /// <param name="val">The boolean value.</param>
        /// <returns>Equivalent scheme boolean.</returns>
        public static SchemeBoolean New(bool val)
        {
            return val ? True : False;
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
        public static SchemeBoolean Equal(SchemeObject obj1, SchemeObject obj2)
        {
            Func<SchemeObject, SchemeObject, SchemeBoolean> action;
            if (equalMap.TryGetValue(obj1.GetType(), out action))
            {
                return action(obj1, obj2);
            }

            // delegate to first member, use C# equality
            return obj1.Equals(obj2) ? True : False;
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
        public static SchemeBoolean Eqv(SchemeObject obj1, SchemeObject obj2)
        {
            return
                New(
                    obj1 == obj2 
                    || (obj1 is SchemeBoolean && ((SchemeBoolean)obj1).Equals(obj2))
                    || (obj1 is Number && ((Number)obj1).Equals(obj2))
                    || (obj1 is Character && ((Character)obj1).Equals(obj2))
                    || (obj1 is Symbol && ((Symbol)obj1).Equals(obj2)));
        }

        /// <summary>
        /// Test an obj to see if it is false.
        /// If the obj is not a boolean, then it will not be false.
        /// </summary>
        /// <param name="value">The obj to test.</param>
        /// <returns>True if the value is a boolean and the boolean is false.</returns>
        public static bool IsFalse(SchemeObject value)
        {
            return value is SchemeBoolean && !((SchemeBoolean)value).Value;
        }

        /// <summary>
        /// Test an obj to see if it is true.
        /// If the obj is not a boolean, then it will not be true.
        /// </summary>
        /// <param name="value">The obj to test.</param>
        /// <returns>True if the value is a boolean and the boolean is true.</returns>
        public static bool IsTrue(SchemeObject value)
        {
            return value is SchemeBoolean && ((SchemeBoolean)value).Value;
        }

        /// <summary>
        /// Test to see if an obj is true.
        /// This is true if the obj is not a boolean, or if it is and is true.
        /// In the other scheme value classes, this method would be called Bool.
        /// </summary>
        /// <param name="obj">The obj to test.</param>
        /// <returns>True if a boolean and true, or else is not a boolean.</returns>
        public static SchemeBoolean Truth(SchemeObject obj)
        {
            return !IsFalse(obj) ? True : False;
        }

        /// <summary>
        /// Converts a boolean into a SchemeBoolean.
        /// </summary>
        /// <param name="b">The boolean value.</param>
        /// <returns>The SchemeBoolean value.</returns>
        public static SchemeBoolean Truth(bool b)
        {
            return b ? True : False;
        }

        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the boolean primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static new void DefinePrimitives(PrimitiveEnvironment env)
        {
            env
                //// <r4rs section="6.1">(boolean? <obj>)</r4rs>
                .DefinePrimitive("boolean?", (args, caller) => Truth(First(args) is SchemeBoolean), 1, ValueType.Obj)
                //// <r4rs section="6.2">(eq? <obj1> <obj2>)</r4rs>
                .DefinePrimitive("eq?", (args, caller) => Truth(Eqv(First(args), Second(args))), 2, ValueType.Obj)
                //// <r4rs section="6.2">(equal? <obj1> <obj2>)</r4rs>
                .DefinePrimitive("equal?", (args, caller) => Truth(Equal(First(args), Second(args))), 2, ValueType.Obj)
                //// <r4rs section="6.2">(eqv? <obj1> <obj2>)</r4rs>
                .DefinePrimitive("eqv?", (args, caller) => Truth(Eqv(First(args), Second(args))), 2, ValueType.Obj)
                //// <r4rs section="6.1">(not <obj>)</r4rs>
                .DefinePrimitive(
                    "not",
                    (args, caller) => Truth(First(args) is SchemeBoolean && ((SchemeBoolean)First(args)).Value == false), 
                    1, 
                    ValueType.Obj)
                //// <r4rs section="6.3">(null? <obj>)</r4rs>
                .DefinePrimitive("null?", (args, caller) => Truth(First(args) is EmptyList), 1, ValueType.Obj);
        }
        #endregion

        /// <summary>
        /// Convert boolean.
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding boolean.</returns>
        public static bool AsBool(SchemeObject x)
        {
            if (x is SchemeBoolean)
            {
                return IsTrue(x);
            }

            ErrorHandlers.TypeError(typeof(SchemeBoolean), x);
            return false;
        }

        #region Equality
        /// <summary>
        /// Provide our own version of the Equals method.
        /// </summary>
        /// <param name="other">The other object.</param>
        /// <returns>True if they are equal boolean values.</returns>
        public override bool Equals(object other)
        {
            if (!(other is SchemeBoolean))
            {
                return false;
            }

            return this.Equals((SchemeBoolean)other);
        }

        /// <summary>
        /// Compares two SchemeBoolean values by comparing their underlying boolean value.
        /// </summary>
        /// <param name="other">The other SchemeBoolean.</param>
        /// <returns>True if they have the same boolean value.</returns>
        public bool Equals(SchemeBoolean other)
        {
            return this.value == other.value;
        }

        /// <summary>
        /// The hash code is the boolean's hash code.
        /// </summary>
        /// <returns>The hash code.</returns>
        public override int GetHashCode()
        {
            return this.value.GetHashCode();
        }

        #endregion

        #region Public Methods
        /// <summary>
        /// Write the boolean to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote (not used).</param>
        /// <param name="buf">The string builder to write to.</param>
        public override void PrintString(bool quoted, StringBuilder buf)
        {
            buf.Append(this.value ? "#t" : "#f");
        }

        /// <summary>
        /// Convert the SchemeBoolean value to a string for printing.
        /// </summary>
        /// <returns>The boolean value as a string.</returns>
        public override string ToString()
        {
            return this.value ? "True" : "False";
        }
        #endregion
    }
}
