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
    public class Vector : ListPrimitives
    {
        #region Constructor
        /// <summary>
        /// Prevents a default instance of the Vector class from being created.
        /// </summary>
        private Vector()
        {
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Check that an object is a vector.
        /// </summary>
        /// <param name="obj">The object.</param>
        /// <returns>The scheme vector.</returns>
        public static Obj[] Vec(object obj)
        {
            if (TypePrimitives.IsVector(obj)) 
            {
                return (Obj[])obj;
            }

            return Vec(ErrorHandlers.TypeError(TypePrimitives.VectorName, obj));
        }

        /// <summary>
        /// Creates the vector from a list of values.
        /// </summary>
        /// <param name="objs">A list of values to put in the vector.</param>
        /// <returns>A vector of the objs.</returns>
        public static Obj[] New(object objs)
        {
            Obj[] vec = new Obj[Length(objs)];

            if (!TypePrimitives.IsPair(objs))
            {
                return vec;
            }

            int i = 0;
            while (TypePrimitives.IsPair(objs))
            {
                vec[i++] = First(objs);
                objs = Rest(objs);
            }

            return vec;
        }

        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the vector primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        internal static void DefinePrimitives(PrimitiveEnvironment env)
        {
            const int MaxInt = int.MaxValue;
            env
                //// <r4rs section="6.8">(list->vector <vector>)</r4rs>
                .DefinePrimitive("list->vector", (args, caller) => New(First(args)), 1)
                //// <r4rs section="6.8">(make-vector <k>)</r4rs>
                //// <r4rs section="6.8">(make-vector <k> <fill>)</r4rs>
                .DefinePrimitive("make-vector", (args, caller) => New(First(args), Second(args)), 1, 2)
                //// <r4rs section="6.8">(vector <obj>)</r4rs>
                .DefinePrimitive("vector", (args, caller) => New(args), 0, MaxInt)
                //// <r4rs section="6.8">(vector->list <vector>)</r4rs>
                .DefinePrimitive("vector->list", (args, caller) => VectorToList(First(args)), 1)
                //// <r4rs section="6.8">(vector-fill! <vector> <fill>)</r4rs>
                .DefinePrimitive("vector-fill", (args, caller) => VectorFill(First(args), Second(args)), 2)
                //// <r4rs section="6.8">(vector-length <vector>)</r4rs>
                .DefinePrimitive("vector-length", (args, caller) => Number.Num(Vec(First(args)).Length), 1)
                //// <r4rs section="6.8">(vector-ref <vector> <k>)</r4rs>
                .DefinePrimitive("vector-ref", (args, caller) => Vec(First(args))[(int)Number.Num(Second(args))], 2)
                //// <r4rs section="6.8">(vector-set <vector> <k> <obj>)</r4rs>
                .DefinePrimitive("vector-set!", (args, caller) => VectorSet(First(args), Second(args), Third(args)), 3)
                //// <r4rs section="6.8">(vector? <obj>)</r4rs>
                .DefinePrimitive("vector?", (args, caller) => SchemeBoolean.Truth(TypePrimitives.IsVector(First(args))), 1);
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Create a vector from a length and an optional fill value.
        /// </summary>
        /// <param name="length">The vector length.</param>
        /// <param name="fill">The value to initialize the vector entries to.</param>
        /// <returns>A vector of the objs filled with the fill object.</returns>
        internal static Obj[] New(object length, object fill)
        {
            Obj[] vec = new object[(int)Number.Num(length)];
            if (TypePrimitives.IsEmptyList(fill))
            {
                fill = Undefined.Instance;
            }

            for (int i = 0; i < vec.Length; i++)
            {
                vec[i] = fill;
            }

            return vec;
        }

        /// <summary>
        /// Tests whether two vectors are equal.
        /// </summary>
        /// <param name="obj1">The first object (must be a scheme vector).</param>
        /// <param name="obj2">The other object.</param>
        /// <returns>True if they are both vectors of equal length and 
        /// all elements are equal.</returns>
        internal static bool Equal(Obj obj1, Obj obj2)
        {
            if (!TypePrimitives.IsVector(obj2))
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
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Set a vector element.
        /// </summary>
        /// <param name="vector">The vector to set.</param>
        /// <param name="k">The index into the vector to set.</param>
        /// <param name="obj">The new value to set.</param>
        /// <returns>Undefined value.</returns>
        private static Obj VectorSet(object vector, object k, object obj)
        {
            Vec(vector)[(int)Number.Num(k)] = obj;
            return Undefined.Instance;
        }

        /// <summary>
        /// Convert a vector into a list of objs.
        /// If the obj is not a vector, return error.
        /// </summary>
        /// <param name="vector">The vector.</param>
        /// <returns>The vector as a list.</returns>
        private static Obj VectorToList(object vector)
        {
            Obj[] vec = Vec(vector);
            Obj result = EmptyList.Instance;
            for (int i = vec.Length - 1; i >= 0; i--)
            {
                result = Cons(vec[i], result);
            }

            return result;
        }

        /// <summary>
        /// Fill all elements of a vector with a given value
        /// </summary>
        /// <param name="vector">The vector to fill.</param>
        /// <param name="fill">The value to fill with.</param>
        /// <returns>Return value is unspecified.</returns>
        private static Obj VectorFill(Obj vector, object fill)
        {
            Obj[] vec = Vec(vector);
            for (int i = 0; i < vec.Length; i++)
            {
                vec[i] = fill;
            }

            return Undefined.Instance;
        }

        #endregion
    }
}
