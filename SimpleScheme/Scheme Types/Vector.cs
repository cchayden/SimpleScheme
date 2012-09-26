// <copyright file="Vector.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Represents a scheme vector.
    /// It has a fixed length and holds arbitrary scheme objects.
    /// </summary>
    public class Vector : IPrintable, ISchemeType
    {
        #region Fields
        /// <summary>
        /// The elements of the vector.
        /// </summary>
        private readonly Obj[] vec;
        #endregion

        #region Constructors
        /// <summary>
        /// Initializes a new instance of the <see cref="Vector"/> class.
        /// </summary>
        /// <param name="length"> The number of elements in the vector.</param>
        private Vector(int length)
        {
            this.vec = new Obj[length];
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Vector"/> class.
        /// </summary>
        /// <param name="length">The vector length.</param>
        /// <param name="fill">The fill.</param>
        private Vector(int length, Obj fill) : this(length)
        {
            for (int i = 0; i < this.vec.Length; i++)
            {
                this.vec[i] = fill;
            }
        }
        #endregion

        #region SchemeType Accessors
        /// <summary>
        /// Gets the name of the type.
        /// </summary>
        public string TypeName
        {
            get { return TypePrimitives.ValueTypeName(TypePrimitives.ValueType.Vector); }
        }
        #endregion

        #region Properties
        /// <summary>
        /// Gets the vector length.
        /// </summary>
        public int Length
        {
            get { return this.vec.Length; }
        }

        /// <summary>
        /// Gets or sets an element of the vector.
        /// </summary>
        /// <param name="index">The index.</param>
        /// <returns>The value at the given index.</returns>
        public Obj this[int index]
        {
            get { return this.vec[index]; }
            set { this.vec[index] = value; }
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Create a new Vector.
        /// </summary>
        /// <param name="count">The size for the new vector.</param>
        /// <returns>The vector.</returns>
        public static Vector New(int count)
        {
            return new Vector(count);
        }

        /// <summary>
        /// Create a vector from a length and an optional fill value.
        /// </summary>
        /// <param name="length">The vector length.</param>
        /// <param name="fill">The value to initialize the vector entries to.</param>
        /// <returns>A vector of the objs filled with the fill object.</returns>
        public static Vector New(Obj length, Obj fill)
        {
            if (fill.IsEmptyList())
            {
                fill = Undefined.New();
            }

            return new Vector(length.AsInt(), fill);
        }

        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the vector primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(PrimitiveEnvironment env)
        {
            const int MaxInt = int.MaxValue;
            env
                //// <r4rs section="6.8">(list->vector <vector>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("list->vector"), 
                        (args, caller) => FromList(args.First()), 
                        1, 
                        TypePrimitives.ValueType.PairOrEmpty)
                //// <r4rs section="6.8">(make-vector <k>)</r4rs>
                //// <r4rs section="6.8">(make-vector <k> <fill>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("make-vector"), 
                        (args, caller) => New(args.First(), args.Second()), 
                        1, 
                        2, 
                        TypePrimitives.ValueType.Number, 
                        TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.8">(vector <obj>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("vector"), 
                        (args, caller) => FromList(args), 
                        0, 
                        MaxInt, 
                        TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.8">(vector->list <vector>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("vector->list"), 
                        (args, caller) => ToList(args.First()), 
                        1, 
                        TypePrimitives.ValueType.Vector)
                //// <r4rs section="6.8">(vector-fill! <vector> <fill>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("vector-fill"), 
                        (args, caller) => Fill(args.First(), args.Second()), 
                        2, 
                        TypePrimitives.ValueType.Vector, 
                        TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.8">(vector-length <vector>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("vector-length"), 
                        (args, caller) => args.First().AsVector().Length, 
                        1, 
                        TypePrimitives.ValueType.Vector)
                //// <r4rs section="6.8">(vector-ref <vector> <k>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("vector-ref"), 
                        (args, caller) => Get(args.First(), args.Second()), 
                        2, 
                        TypePrimitives.ValueType.Vector, 
                        TypePrimitives.ValueType.Number)
                //// <r4rs section="6.8">(vector-set <vector> <k> <obj>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("vector-set!"), 
                        (args, caller) => Set(args.First(), args.Second(), args.Third()), 
                        3, 
                        TypePrimitives.ValueType.Vector, 
                        TypePrimitives.ValueType.Number, 
                        TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.8">(vector? <obj>)</r4rs>
                .DefinePrimitive( 
                        Symbol.New("vector?"), 
                        (args, caller) => SchemeBoolean.Truth(args.First().IsVector()), 
                        1, 
                        TypePrimitives.ValueType.Obj);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Tests whether two vectors are equal.
        /// </summary>
        /// <param name="obj1">The first object (must be a scheme vector).</param>
        /// <param name="obj2">The other object.</param>
        /// <returns>True if they are both vectors of equal length and 
        /// all elements are equal.</returns>
        public static SchemeBoolean Equal(Obj obj1, Obj obj2)
        {
            if (!obj2.IsVector())
            {
                return SchemeBoolean.False;
            }

            var vector1 = obj1.AsVector();
            var vector2 = obj2.AsVector();
            if (vector1.Length != vector2.Length)
            {
                return SchemeBoolean.False;
            }

            for (int i = 0; i < vector1.Length; i++)
            {
                if (!SchemeBoolean.Equal(vector1[i], vector2[i]).Value)
                {
                    return SchemeBoolean.False;
                }
            }

            return SchemeBoolean.True;
        }

        /// <summary>
        /// Get a vector element
        /// </summary>
        /// <param name="vector">The vector to get from.</param>
        /// <param name="k">The index into the vector to get.</param>
        /// <returns>The vector element.</returns>
        public static Obj Get(Obj vector, Obj k)
        {
            return vector.AsVector()[k.AsInt()];
        }

        /// <summary>
        /// Set a vector element.
        /// </summary>
        /// <param name="vector">The vector to set.</param>
        /// <param name="index">The index into the vector to set.</param>
        /// <param name="obj">The new value to set.</param>
        /// <returns>Undefined value.</returns>
        public static Obj Set(Obj vector, Obj index, Obj obj)
        {
            vector.AsVector()[index.AsInt()] = obj;
            return Undefined.New();
        }

        /// <summary>
        /// Creates the vector from a list of values.
        /// </summary>
        /// <param name="objs">A list of values to put in the vector.</param>
        /// <returns>A vector of the objs.</returns>
        public static Vector FromList(Obj objs)
        {
            Vector vec = New(objs.ListLength());
            if (!objs.IsPair())
            {
                return vec;
            }

            int i = 0;
            while (objs.IsPair())
            {
                vec[i++] = objs.First();
                objs = objs.Rest();
            }

            return vec;
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Identifies objects of this scheme type.
        /// </summary>
        /// <param name="obj">The object to test.</param>
        /// <returns>True if the object is this scheme type.</returns>
        public static bool Is(Obj obj)
        {
            return obj is Vector;
        }

        /// <summary>
        /// Write the vector to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public void PrintString(bool quoted, StringBuilder buf)
        {
            buf.Append("#(");
            if (this.vec.Length > 0)
            {
                foreach (Obj v in this.vec)
                {
                    Printer.PrintString(v, quoted, buf);
                    buf.Append(' ');
                }

                buf.Remove(buf.Length - 1, 1);
            }

            buf.Append(')');
        }

        /// <summary>
        /// Clean each element of the vector.
        /// </summary>
        public void Clean()
        {
            foreach (Obj v in this.vec)
            {
                Cleaner.Clean(v);
            }
        }

        /// <summary>
        /// Convert the vector to an attay of objects.
        /// </summary>
        /// <returns>An array of object</returns>
        public object[] AsObjectArray()
        {
            var res = new object[this.vec.Length];
            for (var i = 0; i < res.Length; i++)
            {
                res[i] = this.vec[i];
            }

            return res;
        }

        /// <summary>
        /// Convert the vector to an attay of ints.
        /// </summary>
        /// <returns>An array of int</returns>
        public int[] AsIntArray()
        {
            var res = new int[this.vec.Length];
            for (var i = 0; i < res.Length; i++)
            {
                res[i] = this.vec[i].AsInt();
            }

            return res;
        }

        /// <summary>
        /// Convert the vector to an attay of bools.
        /// </summary>
        /// <returns>An array of bool</returns>
        public bool[] AsBoolArray()
        {
            var res = new bool[this.vec.Length];
            for (var i = 0; i < res.Length; i++)
            {
                res[i] = this.vec[i].AsBoolean();
            }

            return res;
        }

        /// <summary>
        /// Convert the vector to an attay of bytes.
        /// </summary>
        /// <returns>An array of byte</returns>
        public byte[] AsByteArray()
        {
            var res = new byte[this.vec.Length];
            for (var i = 0; i < res.Length; i++)
            {
                res[i] = this.vec[i].AsByte();
            }

            return res;
        }

        /// <summary>
        /// Convert the vector to an attay of shorts.
        /// </summary>
        /// <returns>An array of short</returns>
        public short[] AsShortArray()
        {
            var res = new short[this.vec.Length];
            for (var i = 0; i < res.Length; i++)
            {
                res[i] = this.vec[i].AsShort();
            }

            return res;
        }

        /// <summary>
        /// Convert the vector to an attay of longs.
        /// </summary>
        /// <returns>An array of long</returns>
        public long[] AsLongArray()
        {
            var res = new long[this.vec.Length];
            for (var i = 0; i < res.Length; i++)
            {
                res[i] = this.vec[i].AsLong();
            }

            return res;
        }

        /// <summary>
        /// Convert the vector to an attay of floats.
        /// </summary>
        /// <returns>An array of float</returns>
        public float[] AsFloatArray()
        {
            var res = new float[this.vec.Length];
            for (var i = 0; i < res.Length; i++)
            {
                res[i] = this.vec[i].AsFloat();
            }

            return res;
        }

        /// <summary>
        /// Convert the vector to an attay of doubles.
        /// </summary>
        /// <returns>An array of double</returns>
        public double[] AsDoubleArray()
        {
            var res = new double[this.vec.Length];
            for (var i = 0; i < res.Length; i++)
            {
                res[i] = this.vec[i].AsDouble();
            }

            return res;
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Convert a vector into a list of objs.
        /// If the obj is not a vector, return error.
        /// </summary>
        /// <param name="vector">The vector.</param>
        /// <returns>The vector as a list.</returns>
        internal static Obj ToList(Obj vector)
        {
            Vector vec = vector.AsVector();
            Obj result = EmptyList.New();
            for (int i = vec.Length - 1; i >= 0; i--)
            {
                result = vec[i].Cons(result);
            }

            return result;
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Fill all elements of a vector with a given value
        /// </summary>
        /// <param name="vector">The vector to fill.</param>
        /// <param name="fill">The value to fill with.</param>
        /// <returns>Return value is unspecified.</returns>
        private static Obj Fill(Obj vector, Obj fill)
        {
            Vector vec = vector.AsVector();
            for (int i = 0; i < vec.Length; i++)
            {
                vec[i] = fill;
            }

            return Undefined.New();
        }
        #endregion
    }

    /// <summary>
    /// Extension class for Vector
    /// </summary>
    public static class VectorExtension
    {
        /// <summary>
        /// Tests whether to given object is a scheme vector.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme vector.</returns>
        public static bool IsVector(this Obj obj)
        {
            return Vector.Is(obj);
        }

        /// <summary>
        /// Check that the object is a vector.
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding vector.</returns>
        public static Vector AsVector(this Obj x)
        {
            if (Vector.Is(x))
            {
                return (Vector)x;
            }

            ErrorHandlers.TypeError(typeof(Vector), x);
            return null;
        }
    }    
}
