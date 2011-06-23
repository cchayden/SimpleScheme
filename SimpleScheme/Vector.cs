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
    public sealed class Vector : ListPrimitives
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
            this.vec = new object[Length(objs)];

            if (!(objs is Pair))
            {
                return;
            }

            int i = 0;
            foreach (object elem in (Pair)objs)
            {
                this.vec[i++] = elem;
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
            if (fill == null)
            {
                return;
            }

            for (int i = 0; i < this.vec.Length; i++)
            {
                this.vec[i] = fill;
            }
        }

        /// <summary>
        /// Gets the length of the vector.
        /// </summary>
        public int VectorLength
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
                //// <r4rs section="6.8">(make-vector <k>)</r4rs>
                //// <r4rs section="6.8">(make-vector <k> <fill>)</r4rs>
                .DefinePrimitive("make-vector", (caller, args) => new Vector(First(args), Second(args)), 1, 2)
                //// <r4rs section="6.8">(vector <obj>)</r4rs>
                .DefinePrimitive("vector", (caller, args) => new Vector(args), 0, MaxInt)
                //// <r4rs section="6.8">(vector->list <vector>)</r4rs>
                .DefinePrimitive("vector->list", (caller, args) => VectorToList(First(args)), 1)
                //// <r4rs section="6.8">(vector-fill! <vector> <fill>)</r4rs>
                .DefinePrimitive("vector-fill", (caller, args) => VectorFill(First(args), Second(args)), 2)
                //// <r4rs section="6.8">(vector-length <vector>)</r4rs>
                .DefinePrimitive("vector-length", (caller, args) => Number.Num(Vec(First(args)).VectorLength), 1)
                //// <r4rs section="6.8">(vector-ref <vector> <k>)</r4rs>
                .DefinePrimitive("vector-ref", (caller, args) => Vec(First(args))[(int)Number.Num(Second(args))], 2)
                //// <r4rs section="6.8">(vector-set <vector> <k> <obj>)</r4rs>
                .DefinePrimitive("vector-set!", (caller, args) => Vec(First(args))[(int)Number.Num(Second(args))] = Third(args), 3)
                //// <r4rs section="6.8">(vector? <obj>)</r4rs>
                .DefinePrimitive("vector?", (caller, args) => SchemeBoolean.Truth(First(args) is Vector), 1);
        }

        /// <summary>
        /// Tests whether two vectors are equal.
        /// </summary>
        /// <param name="vector1">One vector.</param>
        /// <param name="vector2">The other vector.</param>
        /// <returns>True if they are both vectors of equal length and 
        /// all elements are equal.</returns>
        public static bool Equal(Vector vector1, Vector vector2)
        {
            if (vector1.VectorLength != vector2.VectorLength)
            {
                return false;
            }

            for (int i = 0; i < vector1.VectorLength; i++)
            {
                if (!SchemeBoolean.Equal(vector1.vec[i], vector2.vec[i]))
                {
                    return false;
                }
            }

            return true;
        }

        /// <summary>
        /// Convert the vector a string.
        /// </summary>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        public void AsString(bool quoted, StringBuilder buf)
        {
            buf.Append("#(");
            if (this.VectorLength > 0)
            {
                for (int i = 0; i < this.vec.Length; i++)
                {
                    SchemeString.AsString(this.vec[i], quoted, buf);
                    buf.Append(' ');
                }

                buf.Remove(buf.Length - 1, 1);
            }

            buf.Append(')');
        }

        /// <summary>
        /// Provide the string representation of a vector.
        /// </summary>
        /// <returns>The vector as a string.</returns>
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();
            this.AsString(false, sb);
            return sb.ToString();
        }

        /// <summary>
        /// Convert an object that should be a vector into an array of objects.
        /// </summary>
        /// <param name="vector">The vector.</param>
        /// <returns>The array of objects.</returns>
        private static Vector Vec(object vector)
        {
            if (vector is Vector) 
            {
                return (Vector)vector;
            }

            return Vec(ErrorHandlers.Error("Expected a vector, got: " + vector));
        }

        /// <summary>
        /// Convert a vector into a list of objects.
        /// If the object is not a vector, return null.
        /// </summary>
        /// <param name="vector">The vector.</param>
        /// <returns>The list, or null.</returns>
        private static Pair VectorToList(object vector)
        {
            if (vector is Vector)
            {
                Vector vec = (Vector)vector;
                Pair result = null;
                for (int i = vec.VectorLength - 1; i >= 0; i--)
                {
                    result = Cons(vec[i], result);
                }

                return result;
            }

            ErrorHandlers.Error("VectorToList: expected a vector, got: " + vector);
            return null;
        }

        /// <summary>
        /// Fill all elements of a vector with a given value
        /// </summary>
        /// <param name="vector">The vector to fill.</param>
        /// <param name="fill">The value to fill with.</param>
        /// <returns>Return value is unspecified.</returns>
        private static object VectorFill(object vector, object fill)
        {
            if (vector is Vector)
            {
                Vector vec = (Vector)vector;
                for (int i = 0; i < vec.VectorLength; i++)
                {
                    vec[i] = fill;
                }

                return false;
            }

            ErrorHandlers.Error("VectorFill: expected a vector, got: " + vector);
            return null;
        }
    }
}
