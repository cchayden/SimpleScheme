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
    public static class Vector
    {
        #region Constants
        /// <summary>
        /// The printable name of the vector type.
        /// </summary>
        public const string Name = "vector";
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Tests whether to given object is a scheme vector.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme vector.</returns>
        public static bool Is(Obj obj)
        {
            return obj is Obj[];
        }

        /// <summary>
        /// Check that an object is a vector.
        /// </summary>
        /// <param name="obj">The object.</param>
        /// <returns>The scheme vector.</returns>
        public static Obj[] As(Obj obj)
        {
            if (Is(obj)) 
            {
                return (Obj[])obj;
            }

            ErrorHandlers.TypeError(Name, obj);
            return null;
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
                .DefinePrimitive("list->vector", (args, caller) => FromList(List.First(args)), 1, Primitive.ValueType.PairOrEmpty)
                //// <r4rs section="6.8">(make-vector <k>)</r4rs>
                //// <r4rs section="6.8">(make-vector <k> <fill>)</r4rs>
                .DefinePrimitive("make-vector", (args, caller) => New(List.First(args), List.Second(args)), 1, 2, Primitive.ValueType.Number, Primitive.ValueType.Obj)
                //// <r4rs section="6.8">(vector <obj>)</r4rs>
                .DefinePrimitive("vector", (args, caller) => FromList(args), 0, MaxInt, Primitive.ValueType.Obj)
                //// <r4rs section="6.8">(vector->list <vector>)</r4rs>
                .DefinePrimitive("vector->list", (args, caller) => ToList(List.First(args)), 1, Primitive.ValueType.Vector)
                //// <r4rs section="6.8">(vector-fill! <vector> <fill>)</r4rs>
                .DefinePrimitive("vector-fill", (args, caller) => Fill(List.First(args), List.Second(args)), 2, Primitive.ValueType.Vector, Primitive.ValueType.Obj)
                //// <r4rs section="6.8">(vector-length <vector>)</r4rs>
                .DefinePrimitive("vector-length", (args, caller) => Number.As(As(List.First(args)).Length), 1, Primitive.ValueType.Vector)
                //// <r4rs section="6.8">(vector-ref <vector> <k>)</r4rs>
                .DefinePrimitive("vector-ref", (args, caller) => Get(List.First(args), List.Second(args)), 2, Primitive.ValueType.Vector, Primitive.ValueType.Number)
                //// <r4rs section="6.8">(vector-set <vector> <k> <obj>)</r4rs>
                .DefinePrimitive("vector-set!", (args, caller) => Set(List.First(args), List.Second(args), List.Third(args)), 3, 
                                Primitive.ValueType.Vector, Primitive.ValueType.Number, Primitive.ValueType.Obj)
                //// <r4rs section="6.8">(vector? <obj>)</r4rs>
                .DefinePrimitive("vector?", (args, caller) => SchemeBoolean.Truth(Is(List.First(args))), 1, Primitive.ValueType.Obj);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Write the vector to the string builder.
        /// </summary>
        /// <param name="vec">The vector.</param>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public static void AsString(Obj[] vec, bool quoted, StringBuilder buf)
        {
            buf.Append("#(");
            if (vec.Length > 0)
            {
                foreach (Obj v in vec)
                {
                    Printer.AsString(v, quoted, buf);
                    buf.Append(' ');
                }

                buf.Remove(buf.Length - 1, 1);
            }

            buf.Append(')');
        }

        /// <summary>
        /// Tests whether two vectors are equal.
        /// </summary>
        /// <param name="obj1">The first object (must be a scheme vector).</param>
        /// <param name="obj2">The other object.</param>
        /// <returns>True if they are both vectors of equal length and 
        /// all elements are equal.</returns>
        public static bool Equal(Obj obj1, Obj obj2)
        {
            if (!Is(obj2))
            {
                return false;
            }

            Obj[] vector1 = (Obj[])obj1;
            Obj[] vector2 = (Obj[])obj2;
            if (vector1.Length != vector2.Length)
            {
                return false;
            }

            for (int i = 0; i < vector1.Length; i++)
            {
                if (!SchemeBoolean.Equal(vector1[i], vector2[i]))
                {
                    return false;
                }
            }

            return true;
        }

        /// <summary>
        /// Create a vector from a length and an optional fill value.
        /// </summary>
        /// <param name="length">The vector length.</param>
        /// <param name="fill">The value to initialize the vector entries to.</param>
        /// <returns>A vector of the objs filled with the fill object.</returns>
        public static Obj[] New(Obj length, Obj fill)
        {
            Obj[] vec = new Obj[(int)Number.As(length)];
            if (EmptyList.Is(fill))
            {
                fill = new Undefined();
            }

            for (int i = 0; i < vec.Length; i++)
            {
                vec[i] = fill;
            }

            return vec;
        }

        /// <summary>
        /// Get a vector element
        /// </summary>
        /// <param name="vector">The vector to get from.</param>
        /// <param name="k">The index into the vector to get.</param>
        /// <returns>The vector element.</returns>
        public static Obj Get(Obj vector, Obj k)
        {
            return As(vector)[(int)Number.As(k)];
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
            As(vector)[(int)Number.As(index)] = obj;
            return new Undefined();
        }

        /// <summary>
        /// Creates the vector from a list of values.
        /// </summary>
        /// <param name="objs">A list of values to put in the vector.</param>
        /// <returns>A vector of the objs.</returns>
        public static Obj[] FromList(Obj objs)
        {
            Obj[] vec = new Obj[List.Length(objs)];
            if (!Pair.Is(objs))
            {
                return vec;
            }

            int i = 0;
            while (Pair.Is(objs))
            {
                vec[i++] = List.First(objs);
                objs = List.Rest(objs);
            }

            return vec;
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
            Obj[] vec = As(vector);
            Obj result = EmptyList.Instance;
            for (int i = vec.Length - 1; i >= 0; i--)
            {
                result = Pair.Cons(vec[i], result);
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
            Obj[] vec = As(vector);
            for (int i = 0; i < vec.Length; i++)
            {
                vec[i] = fill;
            }

            return new Undefined();
        }
        #endregion
    }
}
