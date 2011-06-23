// <copyright file="Vector.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Collections;
    using System.Collections.Generic;

    /// <summary>
    /// Represents a scheme vector.
    /// </summary>
    public sealed class Vector : IEnumerable<object>
    {
        /// <summary>
        /// A vector is an array of objects.
        /// </summary>
        private readonly object[] vec;

        /// <summary>
        /// Initializes a new instance of the Vector class.
        /// Creates the vector from a list of values.
        /// </summary>
        /// <param name="objs">A list of values to put in the vector.</param>
        public Vector(object objs)
        {
            this.vec = new object[List.Length(objs)];

            if (objs is Pair)
            {
                int i = 0;
                foreach (object elem in (Pair)objs)
                {
                    this.vec[i++] = elem;
                }
            }
        }

        /// <summary>
        /// Initializes a new instance of the Vector class.
        /// This is used only in unit tests.
        /// </summary>
        /// <param name="vec">An array of objects.</param>
        public Vector(object[] vec)
        {
            this.vec = vec;
        }

        /// <summary>
        /// Initializes a new instance of the Vector class.
        /// Create a vector from a length and an optional fill value.
        /// </summary>
        /// <param name="length">The vector length.</param>
        /// <param name="fill">The value to initialize the vector entries to.</param>
        public Vector(object length, object fill)
        {
            this.vec = new object[(int)Number.Num(length)];
            if (fill != null)
            {
                for (int i = 0; i < this.vec.Length; i++)
                {
                    this.vec[i] = fill;
                }
            }
        }

        /// <summary>
        /// Gets the length of the vector.
        /// </summary>
        public int Length
        {
            get { return this.vec.Length; }
        }

        /// <summary>
        /// Gets or sets a vector element.
        /// </summary>
        /// <param name="i">The index to get or set.</param>
        /// <returns>The vector element.</returns>
        public object this[int i]
        {
            get
            {
                return this.vec[i];
            }

            set
            {
                this.vec[i] = value;
            }
        }

        /// <summary>
        /// Define the vector primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(Environment env)
        {
            const int MaxInt = int.MaxValue;
            env
                .DefinePrimitive("make-vector", (parent, args) => new Vector(List.First(args), List.Second(args)), 1, 2)
                .DefinePrimitive("vector", (parent, args) => new Vector(args), 0, MaxInt)
                .DefinePrimitive("vector->list", (parent, args) => VectorToList(List.First(args)), 1)
                .DefinePrimitive("vector-length", (parent, args) => Number.Num(Vec(List.First(args)).Length), 1)
                .DefinePrimitive("vector-ref", (parent, args) => Vec(List.First(args))[(int)Number.Num(List.Second(args))], 2)
                .DefinePrimitive("vector-set!", (parent, args) => Vec(List.First(args))[(int)Number.Num(List.Second(args))] = List.Third(args), 3)
                .DefinePrimitive("vector?", (parent, args) => SchemeBoolean.Truth(List.First(args) is Vector), 1);
        }

        /// <summary>
        /// Tests whether two vectors are equal.
        /// </summary>
        /// <param name="xo">One vector.</param>
        /// <param name="yo">The other vector.</param>
        /// <returns>True if they are both vectors and are equal.</returns>
        public static bool Equal(Vector xo, Vector yo)
        {
            if (xo.Length != yo.Length)
            {
                return false;
            }

            for (int i = xo.Length - 1; i >= 0; i--)
            {
                if (!SchemeBoolean.Equal(xo.vec[i], yo.vec[i]))
                {
                    return false;
                }
            }

            return true;
        }

        /// <summary>
        /// Enumerates characters from the vector.
        /// </summary>
        /// <returns>The vector elements.</returns>
        public IEnumerator<object> GetEnumerator()
        {
            foreach (object obj in this.vec)
            {
                yield return obj;
            }
        }

        /// <summary>
        /// Gets the vector enumerator.
        /// </summary>
        /// <returns>The vector enumerator.</returns>
        IEnumerator IEnumerable.GetEnumerator()
        {
            return this.GetEnumerator();
        }

        /// <summary>
        /// Convert an object that should be a vector into an array of objects.
        /// </summary>
        /// <param name="x">The vector.</param>
        /// <returns>The array of objects.</returns>
        private static Vector Vec(object x)
        {
            if (x is Vector) 
            {
                return (Vector)x;
            }

            return Vec(ErrorHandlers.Error("Expected a vector, got: " + x));
        }

        /// <summary>
        /// Convert a vector into a list of objects.
        /// If the object is not a vector, return null.
        /// </summary>
        /// <param name="x">The vector.</param>
        /// <returns>The list, or null.</returns>
        private static Pair VectorToList(object x)
        {
            if (x is Vector)
            {
                Vector vec = (Vector)x;
                Pair result = null;
                for (int i = vec.Length - 1; i >= 0; i--)
                {
                    result = List.Cons(vec[i], result);
                }

                return result;
            }

            ErrorHandlers.Error("VectorToList: expected a vector, got: " + x);
            return null;
        }
    }
}
