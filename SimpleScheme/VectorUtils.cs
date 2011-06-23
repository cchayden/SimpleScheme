// <copyright file="VectorUtils.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// Utilities for vector primitives.
    /// </summary>
    public class VectorUtils : SchemeUtils
    {
        /// <summary>
        /// Turns a list of objects into a vector of the appropriate length.
        /// </summary>
        /// <param name="objs">The list to convert.</param>
        /// <returns>The array of objects (vector).</returns>
        public static object[] ListToVector(object objs)
        {
            object[] vec = new object[Length(objs)];

            for (int i = 0; objs is Pair; i++)
            {
                vec[i] = First(objs);
                objs = Rest(objs);
            }

            return vec;
        }

        /// <summary>
        /// Convert an object that should be a vector into an array of objects.
        /// </summary>
        /// <param name="x">The vector.</param>
        /// <returns>The array of objects.</returns>
        public static object[] Vec(object x)
        {
            try
            {
                return (object[])x;
            }
            catch (InvalidCastException)
            {
                return Vec(Error("expected a vector, got: " + x));
            }
        }

        /// <summary>
        /// Convert a vector into a list of objects.
        /// If the pbject is not a vector, return null.
        /// </summary>
        /// <param name="x">The vector.</param>
        /// <returns>The list, or null.</returns>
        public static Pair VectorToList(object x)
        {
            if (x is object[])
            {
                object[] vec = (object[])x;
                Pair result = null;
                for (int i = vec.Length - 1; i >= 0; i--)
                {
                    result = Cons(vec[i], result);
                }

                return result;
            }

            Error("expected a vector, got: " + x);
            return null;
        }
    }
}
