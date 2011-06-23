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
    public class Vector : ListPrimitives
    {
        /// <summary>
        /// Prevents a default instance of the Vector class from being created.
        /// </summary>
        private Vector()
        {
        }

        /// <summary>
        /// Creates the vector from a list of values.
        /// </summary>
        /// <param name="objs">A list of values to put in the vector.</param>
        /// <returns>A vector of the objects.</returns>
        public static object[] MakeVector(object objs)
        {
            object[] vec = new object[Length(objs)];

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
        /// Create a vector from a length and an optional fill value.
        /// </summary>
        /// <param name="length">The vector length.</param>
        /// <param name="fill">The value to initialize the vector entries to.</param>
        /// <returns>A vector of the objects filled with the fill object.</returns>
        public static object[] MakeVector(object length, object fill)
        {
            object[] vec = new object[(int)Number.Num(length)];
            if (fill == List.Empty)
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
        /// Define the vector primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(Environment env)
        {
            const int MaxInt = int.MaxValue;
            env
                //// <r4rs section="6.8">(list->vector <vector>)</r4rs>
                .DefinePrimitive("list->vector", (caller, args) => MakeVector(First(args)), 1)
                //// <r4rs section="6.8">(make-vector <k>)</r4rs>
                //// <r4rs section="6.8">(make-vector <k> <fill>)</r4rs>
                .DefinePrimitive("make-vector", (caller, args) => MakeVector(First(args), Second(args)), 1, 2)
                //// <r4rs section="6.8">(vector <obj>)</r4rs>
                .DefinePrimitive("vector", (caller, args) => MakeVector(args), 0, MaxInt)
                //// <r4rs section="6.8">(vector->list <vector>)</r4rs>
                .DefinePrimitive("vector->list", (caller, args) => VectorToList(First(args)), 1)
                //// <r4rs section="6.8">(vector-fill! <vector> <fill>)</r4rs>
                .DefinePrimitive("vector-fill", (caller, args) => VectorFill(First(args), Second(args)), 2)
                //// <r4rs section="6.8">(vector-length <vector>)</r4rs>
                .DefinePrimitive("vector-length", (caller, args) => Number.Num(Vec(First(args)).Length), 1)
                //// <r4rs section="6.8">(vector-ref <vector> <k>)</r4rs>
                .DefinePrimitive("vector-ref", (caller, args) => Vec(First(args))[(int)Number.Num(Second(args))], 2)
                //// <r4rs section="6.8">(vector-set <vector> <k> <obj>)</r4rs>
                .DefinePrimitive("vector-set!", (caller, args) => VectorSet(First(args), Second(args), Third(args)), 3)
                //// <r4rs section="6.8">(vector? <obj>)</r4rs>
                .DefinePrimitive("vector?", (caller, args) => SchemeBoolean.Truth(First(args) is object[]), 1);
        }

        /// <summary>
        /// Tests whether two vectors are equal.
        /// </summary>
        /// <param name="vector1">One vector.</param>
        /// <param name="vector2">The other vector.</param>
        /// <returns>True if they are both vectors of equal length and 
        /// all elements are equal.</returns>
        public static bool Equal(object[] vector1, object[] vector2)
        {
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
        /// Convert the vector a string.
        /// </summary>
        /// <param name="vec">The vector to convert.</param>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        public static void AsString(object[] vec, bool quoted, StringBuilder buf)
        {
            buf.Append("#(");
            if (vec.Length > 0)
            {
                foreach (object v in vec)
                {
                    SchemeString.AsString(v, quoted, buf);
                    buf.Append(' ');
                }

                buf.Remove(buf.Length - 1, 1);
            }

            buf.Append(')');
        }

        /// <summary>
        /// Convert an object that should be a vector into an array of objects.
        /// </summary>
        /// <param name="vector">The vector.</param>
        /// <returns>The array of objects.</returns>
        private static object[] Vec(object vector)
        {
            if (vector is object[]) 
            {
                return (object[])vector;
            }

            return Vec(ErrorHandlers.Error("Expected a vector, got: " + vector));
        }

        /// <summary>
        /// Set a vector element.
        /// </summary>
        /// <param name="vector">The vector to set.</param>
        /// <param name="k">The index into the vector to set.</param>
        /// <param name="obj">The new value to set.</param>
        /// <returns>Undefined value.</returns>
        private static object VectorSet(object vector, object k, object obj)
        {
            Vec(vector)[(int)Number.Num(k)] = obj;
            return Undefined.Instance;
        }

        /// <summary>
        /// Convert a vector into a list of objects.
        /// If the object is not a vector, return error.
        /// </summary>
        /// <param name="vector">The vector.</param>
        /// <returns>The list, or undefined.</returns>
        private static object VectorToList(object vector)
        {
            if (vector is object[])
            {
                object[] vec = (object[])vector;
                object result = List.Empty;
                for (int i = vec.Length - 1; i >= 0; i--)
                {
                    result = Cons(vec[i], result);
                }

                return result;
            }

            ErrorHandlers.Error("VectorToList: expected a vector, got: " + vector);
            return List.Empty;
        }

        /// <summary>
        /// Fill all elements of a vector with a given value
        /// </summary>
        /// <param name="vector">The vector to fill.</param>
        /// <param name="fill">The value to fill with.</param>
        /// <returns>Return value is unspecified.</returns>
        private static object VectorFill(object vector, object fill)
        {
            if (vector is object[])
            {
                object[] vec = (object[])vector;
                for (int i = 0; i < vec.Length; i++)
                {
                    vec[i] = fill;
                }

                return Undefined.Instance;
            }

            return ErrorHandlers.Error("VectorFill: expected a vector, got: " + vector);
        }
    }
}
