// <copyright file="SchemeBoolean.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
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
        #endregion

        #region Fields
        /// <summary>
        /// The boolean value.
        /// </summary>
        private readonly bool value;
        #endregion

        #region Constructors
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
            // both empty list
            if (obj1 is EmptyList && obj2 is EmptyList)
            {
                return True;
            }

            if (obj1 is SchemeString)
            {
                return SchemeString.Equal((SchemeString)obj1, obj2);
            }

            if (obj1 is Character)
            {
                return Character.Equal((Character)obj1, obj2);
            }

            if (obj1 is Vector)
            {
                return Vector.Equal((Vector)obj1, obj2);
            }

            if (obj1 is Pair)
            {
                return Pair.Equal((Pair)obj1, obj2);
            }

            if (obj1 is Symbol)
            {
                return Symbol.Equal((Symbol)obj1, obj2);
            }

            if (obj1 is SchemeBoolean)
            {
                return Equal((SchemeBoolean)obj1, obj2);
            }

            if (obj1 is Number && obj2 is Number)
            {
                return Number.Equal((Number)obj1, (Number)obj2);
            }

            // delegate to first member, use C# equality
            return obj1.Equals(obj2) ? True : False;
        }

        /// <summary>
        /// Tests whether two object are equal, where one of them is a SchemeBoolean.
        /// </summary>
        /// <param name="obj1">A SchemeBoolean.</param>
        /// <param name="obj2">Another object.</param>
        /// <returns>True if they are both booleans and have the same value.</returns>
        public static SchemeBoolean Equal(SchemeBoolean obj1, SchemeObject obj2)
        {
            if (!(obj2 is SchemeBoolean))
            {
                return False;
            }

            return obj1.Value == ((SchemeBoolean)obj2).Value ? True : False;
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
            return new SchemeBoolean(obj1 == obj2 || 
                (obj1 is SchemeBoolean && obj2 is SchemeBoolean && ((SchemeBoolean)obj1).Value == ((SchemeBoolean)obj2).Value) || 
                (obj1 is Number && obj2 is Number && ((Number)obj1).N == ((Number)obj2).N) ||
                (obj1 is Character && obj2 is Character && ((Character)obj1).C == ((Character)obj2).C) ||
                (obj1 is Symbol && obj2 is Symbol && obj1.ToString() == obj2.ToString()));
        }

        /// <summary>
        /// Test an obj to see if it is false.
        /// If the obj is not a boolean, then it will not be false.
        /// </summary>
        /// <param name="value">The obj to test.</param>
        /// <returns>True if the value is a boolean and the boolean is false.</returns>
        public static bool IsFalse(SchemeObject value)
        {
            return value is SchemeBoolean && ((SchemeBoolean)value).Value == false;
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
            return new SchemeBoolean(!IsFalse(obj));
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
                return SchemeBoolean.IsTrue(x);
            }

            ErrorHandlers.TypeError(typeof(SchemeBoolean), x);
            return false;
        }

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
