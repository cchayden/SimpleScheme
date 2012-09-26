// <copyright file="Vector.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;

    /// <summary>
    /// Represents a scheme vector.
    /// It has a fixed length and holds arbitrary scheme objects.
    /// </summary>
    public class Vector : IPrintable, ISchemeObject
    {
        #region Fields
        /// <summary>
        /// The elements of the vector.
        /// </summary>
        private readonly ISchemeObject[] vec;
        #endregion

        #region Constructors
        /// <summary>
        /// Initializes a new instance of the <see cref="Vector"/> class.
        /// </summary>
        /// <param name="length"> The number of elements in the vector.</param>
        private Vector(int length)
        {
            this.vec = new ISchemeObject[length];
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Vector"/> class from a length and an optional fill value.
        /// </summary>
        /// <param name="length">The vector length.</param>
        /// <param name="fill">The fill.</param>
        private Vector(int length, ISchemeObject fill) : this(length)
        {
            fill = fill is EmptyList ? Undefined.Instance : fill;
            for (int i = 0; i < this.vec.Length; i++)
            {
                this.vec[i] = fill;
            }
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Vector"/> class from an array of objects.
        /// </summary>
        /// <param name="elems">The vector elements.</param>
        private Vector(object[] elems) : this(elems.Length)
        {
            for (int i = 0; i < this.vec.Length; i++)
            {
                this.vec[i] = ClrObject.FromClrObject(elems[i]);
            }
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Vector"/> class from a length and an optional fill value.
        /// </summary>
        /// <param name="length">The vector length as a scheme object.</param>
        /// <param name="fill">The value to initialize the vector entries to.</param>
        /// <returns>A vector of the objs filled with the fill object.</returns>
        private Vector(ISchemeObject length, ISchemeObject fill) : this(length.AsInt(), fill)
        {
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
        public ISchemeObject this[int index]
        {
            get { return this.vec[index]; }
            set { this.vec[index] = value; }
        }
        #endregion

        #region New
        /// <summary>
        /// Converts an object array into a vector.
        /// </summary>
        /// <param name="elems">The object array.</param>
        /// <returns>The corresponding Vector.</returns>
        public static implicit operator Vector(object[] elems)
        {
            return New(elems);
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Vector"/> class.
        /// </summary>
        /// <param name="length"> The number of elements in the vector.</param>
        /// <returns>A new Vector.</returns>
        public static Vector New(int length)
        {
            return new Vector(length);
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Vector"/> class from a length and an optional fill value.
        /// </summary>
        /// <param name="length">The vector length.</param>
        /// <param name="fill">The fill.</param>
        /// <returns>A new Vector.</returns>
        public static Vector New(int length, ISchemeObject fill)
        {
            return new Vector(length, fill);
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Vector"/> class from an array of objects.
        /// </summary>
        /// <param name="elems">The vector elements.</param>
        /// <returns>A new Vector.</returns>
        public static Vector New(object[] elems)
        {
            return new Vector(elems);
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Vector"/> class from a length and an optional fill value.
        /// </summary>
        /// <param name="length">The vector length as a scheme object.</param>
        /// <param name="fill">The value to initialize the vector entries to.</param>
        /// <returns>A vector of the objs filled with the fill object.</returns>
        /// <returns>A new Vector.</returns>
        public static Vector New(ISchemeObject length, ISchemeObject fill)
        {
            return new Vector(length, fill);
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
                        "list->vector",
                        (args, caller) => FromList(List.First(args)), 
                        1, 
                        TypePrimitives.ValueType.PairOrEmpty)
                //// <r4rs section="6.8">(make-vector <k>)</r4rs>
                //// <r4rs section="6.8">(make-vector <k> <fill>)</r4rs>
                .DefinePrimitive(
                        "make-vector",
                        (args, caller) => New(List.First(args), List.Second(args)), 
                        1, 
                        2, 
                        TypePrimitives.ValueType.Number, 
                        TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.8">(vector <obj>)</r4rs>
                .DefinePrimitive(
                        "vector",
                        (args, caller) => FromList(args), 
                        0, 
                        MaxInt, 
                        TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.8">(vector->list <vector>)</r4rs>
                .DefinePrimitive(
                        "vector->list",
                        (args, caller) => ToList(List.First(args)), 
                        1, 
                        TypePrimitives.ValueType.Vector)
                //// <r4rs section="6.8">(vector-fill! <vector> <fill>)</r4rs>
                .DefinePrimitive(
                        "vector-fill",
                        (args, caller) => Fill(List.First(args), List.Second(args)), 
                        2, 
                        TypePrimitives.ValueType.Vector, 
                        TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.8">(vector-length <vector>)</r4rs>
                .DefinePrimitive(
                        "vector-length",
                        (args, caller) => (Number)List.First(args).AsVector().Length,
                        1, 
                        TypePrimitives.ValueType.Vector)
                //// <r4rs section="6.8">(vector-ref <vector> <k>)</r4rs>
                .DefinePrimitive(
                        "vector-ref",
                        (args, caller) => Get(List.First(args), List.Second(args)), 
                        2, 
                        TypePrimitives.ValueType.Vector, 
                        TypePrimitives.ValueType.Number)
                //// <r4rs section="6.8">(vector-set <vector> <k> <obj>)</r4rs>
                .DefinePrimitive(
                        "vector-set!",
                        (args, caller) => Set(List.First(args), List.Second(args), List.Third(args)), 
                        3, 
                        TypePrimitives.ValueType.Vector, 
                        TypePrimitives.ValueType.Number, 
                        TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.8">(vector? <obj>)</r4rs>
                .DefinePrimitive( 
                        "vector?",
                        (args, caller) => SchemeBoolean.Truth(List.First(args) is Vector), 
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
        public static SchemeBoolean Equal(ISchemeObject obj1, ISchemeObject obj2)
        {
            if (!(obj2 is Vector))
            {
                return false;
            }

            var vector1 = obj1.AsVector();
            var vector2 = obj2.AsVector();
            if (vector1.Length != vector2.Length)
            {
                return false;
            }

            for (int i = 0; i < vector1.Length; i++)
            {
                if (!SchemeBoolean.Equal(vector1[i], vector2[i]).Value)
                {
                    return false;
                }
            }

            return true;
        }

        /// <summary>
        /// Get a vector element
        /// </summary>
        /// <param name="vector">The vector to get from.</param>
        /// <param name="k">The index into the vector to get.</param>
        /// <returns>The vector element.</returns>
        public static ISchemeObject Get(ISchemeObject vector, ISchemeObject k)
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
        public static ISchemeObject Set(ISchemeObject vector, ISchemeObject index, ISchemeObject obj)
        {
            vector.AsVector()[index.AsInt()] = obj;
            return Undefined.Instance;
        }

        /// <summary>
        /// Creates the vector from a list of values.
        /// </summary>
        /// <param name="objs">A list of values to put in the vector.</param>
        /// <returns>A vector of the objs.</returns>
        public static Vector FromList(ISchemeObject objs)
        {
            var vec = new Vector(List.ListLength(objs));
            if (!(objs is Pair))
            {
                return vec;
            }

            int i = 0;
            while (objs is Pair)
            {
                vec[i++] = List.First(objs);
                objs = List.Rest(objs);
            }

            return vec;
        }
        #endregion

        #region Public Methods
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
                foreach (ISchemeObject v in this.vec)
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
            foreach (ISchemeObject v in this.vec)
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
        internal static ISchemeObject ToList(ISchemeObject vector)
        {
            Vector vec = vector.AsVector();
            ISchemeObject result = EmptyList.Instance;
            for (int i = vec.Length - 1; i >= 0; i--)
            {
                result = List.Cons(vec[i], result);
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
        private static ISchemeObject Fill(ISchemeObject vector, ISchemeObject fill)
        {
            Vector vec = vector.AsVector();
            for (int i = 0; i < vec.Length; i++)
            {
                vec[i] = fill;
            }

            return Undefined.Instance;
        }
        #endregion
    }

    /// <summary>
    /// Extension class for Vector
    /// </summary>
    public static class VectorExtension
    {
        /// <summary>
        /// Check that the object is a vector.
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding vector.</returns>
        public static Vector AsVector(this ISchemeObject x)
        {
            if (x is Vector)
            {
                return (Vector)x;
            }

            ErrorHandlers.TypeError(typeof(Vector), x);
            return null;
        }
    }    
}
