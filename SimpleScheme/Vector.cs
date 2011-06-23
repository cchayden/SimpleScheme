// <copyright file="Vector.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Represents a scheme vector.
    /// </summary>
    public class Vector : SchemeUtils
    {
        /// <summary>
        /// A vector is an array of objects.
        /// </summary>
        private readonly object[] vec;

        /// <summary>
        /// Initializes a new instance of the Vector class.
        /// </summary>
        /// <param name="vec">An array of objects.</param>
        public Vector(object[] vec)
        {
            this.vec = vec;
        }

        /// <summary>
        /// Gets the length of the vector.
        /// </summary>
        public new int Length
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
                if (!SchemeUtils.Equal(xo.vec[i], yo.vec[i]))
                {
                    return false;
                }
            }

            return true;
        }

        /// <summary>
        /// Convert an object that should be a vector into an array of objects.
        /// </summary>
        /// <param name="x">The vector.</param>
        /// <returns>The array of objects.</returns>
        public static Vector Vec(object x)
        {
            if (x is Vector) 
            {
                return (Vector)x;
            }

            return Vec(Error("Expected a vector, got: " + x));
        }

        /// <summary>
        /// Turns a list of objects into a vector of the appropriate length.
        /// </summary>
        /// <param name="objs">The list to convert.</param>
        /// <returns>The array of objects (vector).</returns>
        public static Vector ListToVector(object objs)
        {
            object[] vec = new object[SchemeUtils.Length(objs)];

            for (int i = 0; objs is Pair; i++)
            {
                vec[i] = First(objs);
                objs = Rest(objs);
            }

            return new Vector(vec);
        }

        /// <summary>
        /// Convert a vector into a list of objects.
        /// If the object is not a vector, return null.
        /// </summary>
        /// <param name="x">The vector.</param>
        /// <returns>The list, or null.</returns>
        public static Pair VectorToList(object x)
        {
            if (x is Vector)
            {
                Vector vec = (Vector)x;
                Pair result = null;
                for (int i = vec.Length - 1; i >= 0; i--)
                {
                    result = Cons(vec[i], result);
                }

                return result;
            }

            Error("VectorToList: expected a vector, got: " + x);
            return null;
        }
    }
}
