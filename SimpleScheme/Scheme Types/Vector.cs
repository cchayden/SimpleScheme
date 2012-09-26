// <copyright file="Vector.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Diagnostics.Contracts;
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
            Contract.Requires(length >= 0);
            this.vec = new SchemeObject[length];
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Vector"/> class.
        /// </summary>
        /// <param name="length"> The number of elements in the vector.</param>
        /// <param name="lineNumber">The line where the vector is read.</param>
        private Vector(int length, int lineNumber) : base(lineNumber)
        {
            Contract.Requires(length >= 0);
            this.vec = new SchemeObject[length];
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Vector"/> class from a length and an optional fill value.
        /// </summary>
        /// <param name="length">The vector length.</param>
        /// <param name="fill">The fill.</param>
        private Vector(int length, SchemeObject fill) : this(length)
        {
            Contract.Requires(length >= 0);
            Contract.Requires(fill != null);
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
            Contract.Requires(elems != null);
            Contract.Requires(Contract.ForAll(elems, elem => elem != null));
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
            Contract.Requires(length != null);
            Contract.Requires(Number.AsInt(length) >= 0);
            Contract.Requires(fill != null);
        }
        #endregion

        #region Properties
        /// <summary>
        /// Gets the vector length.
        /// </summary>
        public int Length
        {
            get
            {
                Contract.Ensures(Contract.Result<int>() >= 0);
                return this.vec.Length;
            }
        }

        /// <summary>
        /// Gets or sets an element of the vector.
        /// </summary>
        /// <param name="index">The index.</param>
        /// <returns>The value at the given index.</returns>
        public SchemeObject this[int index]
        {
            get
            {
                Contract.Requires(index >= 0);
                Contract.Ensures(Contract.Result<SchemeObject>() != null);
                Contract.Assume(index < this.vec.Length);
                return this.vec[index];
            }

            set
            {
                Contract.Requires(index >= 0);
                Contract.Requires(value != null);
                Contract.Assume(index < this.vec.Length);
                this.vec[index] = value;
            }
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
            Contract.Requires(elems != null);
            return New(elems);
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Vector"/> class.
        /// </summary>
        /// <param name="length"> The number of elements in the vector.</param>
        /// <returns>A new Vector.</returns>
        public static Vector New(int length)
        {
            Contract.Requires(length >= 0);
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
            Contract.Requires(length >= 0);
            Contract.Requires(fill != null);
            return new Vector(length, fill);
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Vector"/> class from an array of objects.
        /// </summary>
        /// <param name="elems">The vector elements.</param>
        /// <returns>A new Vector.</returns>
        public static Vector New(object[] elems)
        {
            Contract.Requires(elems != null);
            Contract.Requires(Contract.ForAll(elems, elem => elem != null));
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
            Contract.Requires(length != null);
            Contract.Requires(Number.AsInt(length) >= 0);
            Contract.Requires(fill != null);
            return new Vector(length, fill);
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Vector"/> class from an object.
        /// This object must be an array of CLR objects.
        /// Each element is converted to a Scheme object.
        /// </summary>
        /// <param name="array">The vector elements.</param>
        /// <returns>A new Vector.</returns>
        public static Vector New(object array)
        {
            Contract.Requires(array != null);
            Type objType = array.GetType();
            if (!objType.IsArray)
            {
                var msg = string.Format("Type conversion failed.  Expected array, got {0}", objType);
                ErrorHandlers.ClrError(msg);
            }

            var ar = (Array)array;
            var v = new Vector(ar.Length);
            for (int i = 0; i < ar.Length; i++)
            {
                var value = ar.GetValue(i) ?? Undefined.Instance;
                v[i] = ClrObject.FromClrObject(value);
            }

            return v;
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
            Contract.Requires(obj1 != null);
            Contract.Requires(obj2 != null);
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
        /// Creates the vector from a list of values.
        /// </summary>
        /// <param name="objs">A list of values to put in the vector.</param>
        /// <returns>A vector of the objs.</returns>
        public static Vector FromList(SchemeObject objs)
        {
            Contract.Requires(objs != null);
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

        /// <summary>
        /// Creates the vector from a list of values.
        /// </summary>
        /// <param name="objs">A list of values to put in the vector.</param>
        /// <param name="lineNumber">The line where the vector is read.</param>
        /// <returns>A vector of the objs.</returns>
        public static Vector FromList(SchemeObject objs, int lineNumber)
        {
            Contract.Requires(objs != null);
            Contract.Ensures(Contract.Result<Vector>() != null);
            var vec = new Vector(ListLength(objs), lineNumber);
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
            Contract.Requires(x != null);
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
            Contract.Requires(x != null);
            if (x is Vector)
            {
                return ((Vector)x).AsArray(Number.AsInt);
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
            Contract.Requires(x != null);
            if (x is Vector)
            {
                return ((Vector)x).AsArray(SchemeBoolean.AsBool);
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
            Contract.Requires(x != null);
            if (x is Vector)
            {
                return ((Vector)x).AsArray(Number.AsByte);
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
            Contract.Requires(x != null);
            if (x is Vector)
            {
                return ((Vector)x).AsArray(Number.AsShort);
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
            Contract.Requires(x != null);
            if (x is Vector)
            {
                return ((Vector)x).AsArray(Number.AsLong);
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
            Contract.Requires(x != null);
            if (x is Vector)
            {
                return ((Vector)x).AsArray(Number.AsFloat);
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
            Contract.Requires(x != null);
            if (x is Vector)
            {
                return ((Vector)x).AsArray(Number.AsDouble);
            }

            ErrorHandlers.TypeError(typeof(Vector), x);
            return null;
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Display the vector as a string.
        /// </summary>
        /// <returns>The vector as a string.</returns>
        public override string ToString()
        {
            return this.ToString(false);
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the vector primitives.
        /// </summary>
        /// <param name="primEnv">The environment to define the primitives into.</param>
        internal static new void DefinePrimitives(PrimitiveEnvironment primEnv)
        {
            Contract.Requires(primEnv != null);
            const int MaxInt = int.MaxValue;
            primEnv
                .DefinePrimitive(
                        "list->vector", 
                        new[] { "6.8", "(list->vector <vector>)" },
                        (args, env, caller) => FromList(First(args)), 
                        new ArgsInfo(1, ArgType.PairOrEmpty))
                .DefinePrimitive(
                        "make-vector", 
                        new[] { "6.8", "(make-vector <k>)", "(make-vector <k> <fill>)" },
                        (args, env, caller) => New(First(args), Second(args)), 
                        new ArgsInfo(1, 2, ArgType.Number, ArgType.Obj))
                .DefinePrimitive(
                        "vector", 
                        new[] { "6.8", "(vector <obj> ...)" },
                        (args, env, caller) => FromList(args), 
                        new ArgsInfo(0, MaxInt,  ArgType.Obj))
                .DefinePrimitive(
                        "vector->list", 
                        new[] { "6.8", "(vector->list <vector>)" },
                        (args, env, caller) => ToList((Vector)First(args)), 
                        new ArgsInfo(1, ArgType.Vector))
                .DefinePrimitive(
                        "vector-fill", 
                        new[] { "6.8", "(vector-fill! <vector> <fill>)" },
                        (args, env, caller) => Fill((Vector)First(args), Second(args)), 
                        new ArgsInfo(2, ArgType.Vector, ArgType.Obj))
                .DefinePrimitive(
                        "vector-length", 
                        new[] { "6.8", "(vector-length <vector>)" },
                        (args, env, caller) => (Number)((Vector)First(args)).Length,
                        new ArgsInfo(1, ArgType.Vector))
                .DefinePrimitive(
                        "vector-ref", 
                        new[] { "6.8", "(vector-ref <vector> <k>)" },
                        (args, env, caller) => Get((Vector)First(args), Second(args)), 
                        new ArgsInfo(2, ArgType.Vector, ArgType.Number))
                .DefinePrimitive(
                        "vector-set!", 
                        new[] { "6.8", "(vector-set <vector> <k> <obj>)" },
                        (args, env, caller) => Set((Vector)First(args), Second(args), Third(args)), 
                        new ArgsInfo(3, ArgType.Vector, ArgType.Number, ArgType.Obj))
                .DefinePrimitive( 
                        "vector?", 
                        new[] { "6.8", "(vector? <obj>)" },
                        (args, env, caller) => SchemeBoolean.Truth(First(args) is Vector), 
                        new ArgsInfo(1, ArgType.Obj));
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
            Contract.Requires(vector != null);
            SchemeObject result = EmptyList.Instance;
            for (int i = vector.Length - 1; i >= 0; i--)
            {
                result = Cons(vector[i], result);
            }

            return result;
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Write the vector to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <returns>The vector as a string.</returns>
        internal override string ToString(bool quoted)
        {
            var buf = new StringBuilder();
            buf.Append("#(");
            if (this.vec.Length > 0)
            {
                foreach (SchemeObject v in this.vec)
                {
                    buf.Append(v.ToString(quoted));
                    buf.Append(' ');
                }

                buf.Remove(buf.Length - 1, 1);
            }

            buf.Append(')');
            return buf.ToString();
        }

        /// <summary>
        /// Clean each element of the vector.
        /// </summary>
        internal override void Clean()
        {
            foreach (SchemeObject v in this.vec)
            {
                v.Clean();
            }
        }

        /// <summary>
        /// Convert the vector to an array of some specified CLR type. with the
        ///   help of a converter function.
        /// </summary>
        /// <typeparam name="T">The type of the elements.</typeparam>
        /// <param name="convert">A converter function.</param>
        /// <returns>An array of the specified type.</returns>
        internal T[] AsArray<T>(Func<SchemeObject, T> convert)
        {
            Contract.Requires(convert != null);
            var res = new T[this.vec.Length];
            for (var i = 0; i < res.Length; i++)
            {
                res[i] = convert(this.vec[i]);
            }

            return res;
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
            Contract.Requires(vector != null);
            Contract.Requires(fill != null);
            for (int i = 0; i < vector.Length; i++)
            {
                vector[i] = fill;
            }

            return Undefined.Instance;
        }

        /// <summary>
        /// Get a vector element
        /// </summary>
        /// <param name="vector">The vector to get from.</param>
        /// <param name="k">The index into the vector to get.</param>
        /// <returns>The vector element.</returns>
        private static SchemeObject Get(Vector vector, SchemeObject k)
        {
            Contract.Requires(vector != null);
            Contract.Requires(k != null);
            Contract.Requires(Number.AsInt(k) >= 0);
            return vector[Number.AsInt(k)];
        }

        /// <summary>
        /// Set a vector element.
        /// </summary>
        /// <param name="vector">The vector to set.</param>
        /// <param name="index">The index into the vector to set.</param>
        /// <param name="obj">The new value to set.</param>
        /// <returns>Undefined value.</returns>
        private static SchemeObject Set(Vector vector, SchemeObject index, SchemeObject obj)
        {
            Contract.Requires(vector != null);
            Contract.Requires(index != null);
            Contract.Requires(Number.AsInt(index) >= 0);
            Contract.Requires(obj != null);
            vector[Number.AsInt(index)] = obj;
            return Undefined.Instance;
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.vec != null);
        }
        #endregion
    }
}
