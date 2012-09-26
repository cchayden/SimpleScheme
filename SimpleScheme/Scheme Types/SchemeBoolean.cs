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
    public class SchemeBoolean : IPrintable
    {
        #region Static Fields
        #region Constants
        /// <summary>
        /// The printable name of the scheme boolean type.
        /// </summary>
        public const string Name = "boolean";
        #endregion

        /// <summary>
        /// Define the true value.
        /// </summary>
        public static readonly SchemeBoolean True = new SchemeBoolean(true);

        /// <summary>
        /// Define the false value.
        /// </summary>
        public static readonly SchemeBoolean False = new SchemeBoolean(false);
        #endregion

        /// <summary>
        /// The printable name of this scheme type.
        /// </summary>
        public static string TypeName = Primitive.ValueType.Boolean.ToString();

        /// <summary>
        /// Identifies objects of this scheme type.
        /// </summary>
        /// <param name="obj">The object to test.</param>
        /// <returns>True if the object is this scheme type.</returns>
        public static bool Is(Obj obj)
        {
            return obj is SchemeBoolean;
        }

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

        #region Accessors
        /// <summary>
        /// Gets a value indicating whether the boolean value is true.
        /// </summary>
        public bool Value
        {
            get { return this.value; }
        }
        #endregion

        #region Public Static Methods
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
        public static SchemeBoolean Equal(Obj obj1, Obj obj2)
        {
            // both empty list
            if (obj1.IsEmptyList() && obj2.IsEmptyList())
            {
                return True;
            }

            if (obj1.IsSchemeString())
            {
                return SchemeString.Equal(obj1, obj2);
            }

            if (obj1.IsCharacter())
            {
                return Character.Equal(obj1, obj2);
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

            if (obj1.IsSchemeBoolean())
            {
                return Equal(obj1.AsSchemeBoolean(), obj2);
            }

            if (obj1.IsNumber() && obj2.IsNumber())
            {
                return Number.Equal(obj1, obj2);
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
        public static SchemeBoolean Equal(SchemeBoolean obj1, Obj obj2)
        {
            if (!obj2.IsSchemeBoolean())
            {
                return False;
            }

            return obj1.Value == obj2.AsSchemeBoolean().Value ? True : False;
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
        public static SchemeBoolean Eqv(Obj obj1, Obj obj2)
        {
            return new SchemeBoolean(obj1 == obj2 || 
                (obj1 is SchemeBoolean && obj2 is SchemeBoolean && obj1.AsSchemeBoolean().Value == obj2.AsSchemeBoolean().Value) || 
                (obj1 is Number && obj2 is Number && obj1.AsNumber().N == obj2.AsNumber().N) ||
                (obj1 is Character && obj2 is Character && obj1.AsCharacter().C == obj2.AsCharacter().C) ||
                (obj1 != null && obj2 != null && obj1.IsSymbol() && obj2.IsSymbol() && obj1.ToString() == obj2.ToString()));
        }

        /// <summary>
        /// Test an obj to see if it is false.
        /// If the obj is not a boolean, then it will not be false.
        /// </summary>
        /// <param name="value">The obj to test.</param>
        /// <returns>True if the value is a boolean and the boolean is false.</returns>
        public static bool IsFalse(Obj value)
        {
            return value.IsSchemeBoolean() && value.AsSchemeBoolean().Value == false;
        }

        /// <summary>
        /// Test an obj to see if it is true.
        /// If the obj is not a boolean, then it will not be true.
        /// </summary>
        /// <param name="value">The obj to test.</param>
        /// <returns>True if the value is a boolean and the boolean is true.</returns>
        public static bool IsTrue(Obj value)
        {
            return value.IsSchemeBoolean() && value.AsSchemeBoolean().Value;
        }

        /// <summary>
        /// Test to see if an obj is true.
        /// This is true if the obj is not a boolean, or if it is and is true.
        /// In the other scheme value classes, this method would be called Bool.
        /// </summary>
        /// <param name="obj">The obj to test.</param>
        /// <returns>True if a boolean and true, or else is not a boolean.</returns>
        public static SchemeBoolean Truth(Obj obj)
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
                .DefinePrimitive(Symbol.New("not"), (args, caller) => Truth(args.First().IsSchemeBoolean() && args.First().AsSchemeBoolean().Value == false), 1, Primitive.ValueType.Obj)
                //// <r4rs section="6.3">(null? <obj>)</r4rs>
                .DefinePrimitive(Symbol.New("null?"), (args, caller) => Truth(args.First().IsEmptyList()), 1, Primitive.ValueType.Obj);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write the boolean to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote (not used).</param>
        /// <param name="buf">The string builder to write to.</param>
        public void PrintString(bool quoted, StringBuilder buf)
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

    #region Extension Class
    /// <summary>
    /// Extension class for SchemeBoolean
    /// </summary>
    public static class SchemeBooleanExtension
    {
        /// <summary>
        /// Tests whether to given object is a scheme boolean.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme boolean.</returns>
        public static bool IsSchemeBoolean(this Obj obj)
        {
            return SchemeBoolean.Is(obj);
        }

        /// <summary>
        /// Convert to scheme boolean.
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding boolean.</returns>
        public static SchemeBoolean AsSchemeBoolean(this Obj x)
        {
            if (SchemeBoolean.Is(x))
            {
                return (SchemeBoolean)x;
            }

            ErrorHandlers.TypeError(SchemeBoolean.Name, x);
            return null;
        }

        /// <summary>
        /// Convertboolean.
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding boolean.</returns>
        public static bool AsBoolean(this Obj x)
        {
            if (SchemeBoolean.Is(x))
            {
                return SchemeBoolean.IsTrue((SchemeBoolean)x);
            }

            ErrorHandlers.TypeError(SchemeBoolean.Name, x);
            return false;
        }
    }
    #endregion   
}
