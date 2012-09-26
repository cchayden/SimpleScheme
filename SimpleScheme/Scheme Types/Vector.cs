// <copyright file="Vector.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Text;

    /// <summary>
    /// Represents a scheme vector.
    /// It has a fixed length and holds arbitrary scheme objects.
    /// </summary>
    public class Vector : SchemeObject
    {
        #region Fields
        /// <summary>
        /// The elements of the vector.
        /// </summary>
        private readonly SchemeObject[] vec;
        #endregion

        #region Constructors
        /// <summary>
        /// Initializes a new instance of the <see cref="Vector"/> class.
        /// </summary>
        /// <param name="length"> The number of elements in the vector.</param>
        private Vector(int length)
        {
            this.vec = new SchemeObject[length];
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Vector"/> class from a length and an optional fill value.
        /// </summary>
        /// <param name="length">The vector length.</param>
        /// <param name="fill">The fill.</param>
        private Vector(int length, SchemeObject fill) : this(length)
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
        private Vector(SchemeObject length, SchemeObject fill) : this(Number.AsInt(length), fill)
        {
        }

        #endregion

        #region SchemeType Accessors
        /// <summary>
        /// Gets the name of the type.
        /// </summary>
        public override string TypeName
        {
            get { return ValueTypeName(ValueType.Vector); }
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
        public SchemeObject this[int index]
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
        public static Vector New(int length, SchemeObject fill)
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
        public static Vector New(SchemeObject length, SchemeObject fill)
        {
            return new Vector(length, fill);
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the vector primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static new void DefinePrimitives(PrimitiveEnvironment env)
        {
            const int MaxInt = int.MaxValue;
            env
                //// <r4rs section="6.8">(list->vector <vector>)</r4rs>
                .DefinePrimitive(
                        "list->vector",
                        (args, caller) => FromList(First(args)), 
                        1, 
                        ValueType.PairOrEmpty)
                //// <r4rs section="6.8">(make-vector <k>)</r4rs>
                //// <r4rs section="6.8">(make-vector <k> <fill>)</r4rs>
                .DefinePrimitive(
                        "make-vector",
                        (args, caller) => New(First(args), Second(args)), 
                        1, 
                        2, 
                        ValueType.Number, 
                        ValueType.Obj)
                //// <r4rs section="6.8">(vector <obj>)</r4rs>
                .DefinePrimitive(
                        "vector",
                        (args, caller) => FromList(args), 
                        0, 
                        MaxInt, 
                        ValueType.Obj)
                //// <r4rs section="6.8">(vector->list <vector>)</r4rs>
                .DefinePrimitive(
                        "vector->list",
                        (args, caller) => ToList((Vector)First(args)), 
                        1, 
                        ValueType.Vector)
                //// <r4rs section="6.8">(vector-fill! <vector> <fill>)</r4rs>
                .DefinePrimitive(
                        "vector-fill",
                        (args, caller) => Fill((Vector)First(args), Second(args)), 
                        2, 
                        ValueType.Vector, 
                        ValueType.Obj)
                //// <r4rs section="6.8">(vector-length <vector>)</r4rs>
                .DefinePrimitive(
                        "vector-length",
                        (args, caller) => (Number)((Vector)First(args)).Length,
                        1, 
                        ValueType.Vector)
                //// <r4rs section="6.8">(vector-ref <vector> <k>)</r4rs>
                .DefinePrimitive(
                        "vector-ref",
                        (args, caller) => Get((Vector)First(args), Second(args)), 
                        2, 
                        ValueType.Vector, 
                        ValueType.Number)
                //// <r4rs section="6.8">(vector-set <vector> <k> <obj>)</r4rs>
                .DefinePrimitive(
                        "vector-set!",
                        (args, caller) => Set((Vector)First(args), Second(args), Third(args)), 
                        3, 
                        ValueType.Vector, 
                        ValueType.Number, 
                        ValueType.Obj)
                //// <r4rs section="6.8">(vector? <obj>)</r4rs>
                .DefinePrimitive( 
                        "vector?",
                        (args, caller) => SchemeBoolean.Truth(First(args) is Vector), 
                        1, 
                        ValueType.Obj);
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
        public static SchemeBoolean Equal(Vector obj1, SchemeObject obj2)
        {
            if (!(obj2 is Vector))
            {
                return false;
            }

            var vector1 = obj1;
            var vector2 = (Vector)obj2;
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
        public static SchemeObject Get(Vector vector, SchemeObject k)
        {
            return vector[Number.AsInt(k)];
        }

        /// <summary>
        /// Set a vector element.
        /// </summary>
        /// <param name="vector">The vector to set.</param>
        /// <param name="index">The index into the vector to set.</param>
        /// <param name="obj">The new value to set.</param>
        /// <returns>Undefined value.</returns>
        public static SchemeObject Set(Vector vector, SchemeObject index, SchemeObject obj)
        {
            vector[Number.AsInt(index)] = obj;
            return Undefined.Instance;
        }

        /// <summary>
        /// Creates the vector from a list of values.
        /// </summary>
        /// <param name="objs">A list of values to put in the vector.</param>
        /// <returns>A vector of the objs.</returns>
        public static Vector FromList(SchemeObject objs)
        {
            var vec = new Vector(ListLength(objs));
            if (!(objs is Pair))
            {
                return vec;
            }

            int i = 0;
            while (objs is Pair)
            {
                vec[i++] = First(objs);
                objs = Rest(objs);
            }

            return vec;
        }
        #endregion

        #region CLR Type Converters
        /// <summary>
        /// Convert a vector to an array of object.
        /// </summary>
        /// <param name="x">The vector.</param>
        /// <returns>The resulting array of objects.</returns>
        public static object[] AsObjectArray(SchemeObject x)
        {
            if (x is Vector)
            {
                return ((Vector)x).AsArray<object>(elem => elem);
            }

            ErrorHandlers.TypeError(typeof(Vector), x);
            return null;
        }

        /// <summary>
        /// Convert a vector to an array of int.
        /// </summary>
        /// <param name="x">The vector.</param>
        /// <returns>The resulting array of int.</returns>
        public static int[] AsIntArray(SchemeObject x)
        {
            if (x is Vector)
            {
                return ((Vector)x).AsArray(elem => Number.AsInt(elem));
            }

            ErrorHandlers.TypeError(typeof(Vector), x);
            return null;
        }

        /// <summary>
        /// Convert a vector to an array of boolean.
        /// </summary>
        /// <param name="x">The vector.</param>
        /// <returns>The resulting array of boolean.</returns>
        public static bool[] AsBoolArray(SchemeObject x)
        {
            if (x is Vector)
            {
                return ((Vector)x).AsArray(elem => SchemeBoolean.AsBool(elem));
            }

            ErrorHandlers.TypeError(typeof(Vector), x);
            return null;
        }

        /// <summary>
        /// Convert a vector to an array of byte.
        /// </summary>
        /// <param name="x">The vector.</param>
        /// <returns>The resulting array of byte.</returns>
        public static byte[] AsByteArray(SchemeObject x)
        {
            if (x is Vector)
            {
                return ((Vector)x).AsArray(elem => Number.AsByte(elem));
            }

            ErrorHandlers.TypeError(typeof(Vector), x);
            return null;
        }

        /// <summary>
        /// Convert a vector to an array of short.
        /// </summary>
        /// <param name="x">The vector.</param>
        /// <returns>The resulting array of short.</returns>
        public static short[] AsShortArray(SchemeObject x)
        {
            if (x is Vector)
            {
                return ((Vector)x).AsArray(elem => Number.AsShort(elem));
            }

            ErrorHandlers.TypeError(typeof(Vector), x);
            return null;
        }

        /// <summary>
        /// Convert a vector to an array of long.
        /// </summary>
        /// <param name="x">The vector.</param>
        /// <returns>The resulting array of long.</returns>
        public static long[] AsLongArray(SchemeObject x)
        {
            if (x is Vector)
            {
                return ((Vector)x).AsArray(elem => Number.AsLong(elem));
            }

            ErrorHandlers.TypeError(typeof(Vector), x);
            return null;
        }

        /// <summary>
        /// Convert a vector to an array float.
        /// </summary>
        /// <param name="x">The vector.</param>
        /// <returns>The resulting array of float.</returns>
        public static float[] AsFloatArray(SchemeObject x)
        {
            if (x is Vector)
            {
                return ((Vector)x).AsArray(elem => Number.AsFloat(elem));
            }

            ErrorHandlers.TypeError(typeof(Vector), x);
            return null;
        }

        /// <summary>
        /// Convert a vector to an array of double.
        /// </summary>
        /// <param name="x">The vector.</param>
        /// <returns>The resulting array of double.</returns>
        public static double[] AsDoubleArray(SchemeObject x)
        {
            if (x is Vector)
            {
                return ((Vector)x).AsArray(elem => Number.AsDouble(elem));
            }

            ErrorHandlers.TypeError(typeof(Vector), x);
            return null;
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write the vector to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public override void PrintString(bool quoted, StringBuilder buf)
        {
            buf.Append("#(");
            if (this.vec.Length > 0)
            {
                foreach (SchemeObject v in this.vec)
                {
                    v.PrintString(quoted, buf);
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
            foreach (SchemeObject v in this.vec)
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
        /// Convert the vector to an array of some specified CLR type. with the
        ///   help of a converter function.
        /// </summary>
        /// <typeparam name="T">The type of the elements.</typeparam>
        /// <param name="convert">A converter function.</param>
        /// <returns>An array of the specified type.</returns>
        public T[] AsArray<T>(Func<SchemeObject, T> convert)
        {
            var res = new T[this.vec.Length];
            for (var i = 0; i < res.Length; i++)
            {
                res[i] = convert(this.vec[i]);
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
        internal static SchemeObject ToList(Vector vector)
        {
            SchemeObject result = EmptyList.Instance;
            for (int i = vector.Length - 1; i >= 0; i--)
            {
                result = Cons(vector[i], result);
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
        private static SchemeObject Fill(Vector vector, SchemeObject fill)
        {
            for (int i = 0; i < vector.Length; i++)
            {
                vector[i] = fill;
            }

            return Undefined.Instance;
        }
        #endregion
    }
}
