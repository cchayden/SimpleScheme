// <copyright file="ListPrimitives.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// Common list primitives used widely.
    /// This is designed to serve as a base class for those that want to use
    ///   these functions without qualification.
    /// Because of this, it has no member variables.
    /// </summary>
    public class ListPrimitives
    {
        /// <summary>
        /// Create a list from an object.
        /// Makes a list of length 1.
        /// </summary>
        /// <param name="a">The object to put into the list.</param>
        /// <returns>The Pair making up the head of the list.</returns>
        public static Pair MakeList(object a)
        {
            return new Pair(a, List.Empty);
        }

        /// <summary>
        /// Create an empty list.
        /// This is actually a Pair whose First is null.
        /// This is not the same as List.Empty.
        /// Used where we need a list to start with, the first cell is normally trimmed off later.
        /// </summary>
        /// <returns>A list whose first cell is null.</returns>
        public static Pair MakeEmptyList()
        {
            return new Pair(List.Empty, List.Empty);
        }

        /// <summary>
        /// Create a list from two objects.
        /// Makes a list of length 2.
        /// </summary>
        /// <param name="a">The first object.</param>
        /// <param name="b">The second object.</param>
        /// <returns>The Pair making up the head of the list.</returns>
        public static Pair MakeList(object a, object b)
        {
            return new Pair(a, new Pair(b, List.Empty));
        }

        /// <summary>
        /// Create a list from three objects.
        /// Makes a list of length 3.
        /// </summary>
        /// <param name="a">The first object.</param>
        /// <param name="b">The second object.</param>
        /// <param name="c">The third object.</param>
        /// <returns>The Pair making up the head of the list.</returns>
        public static Pair MakeList(object a, object b, object c)
        {
            return new Pair(a, new Pair(b, new Pair(c, List.Empty)));
        }

        /// <summary>
        /// Create a list from four objects.
        /// Makes a list of length 4.
        /// </summary>
        /// <param name="a">The first object.</param>
        /// <param name="b">The second object.</param>
        /// <param name="c">The third object.</param>
        /// <param name="d">The fourth object.</param>
        /// <returns>The Pair making up the head of the list.</returns>
        public static Pair MakeList(object a, object b, object c, object d)
        {
            return new Pair(a, new Pair(b, new Pair(c, new Pair(d, List.Empty))));
        }

        /// <summary>
        /// Create a list made out of all the objects given.
        /// </summary>
        /// <param name="args">The object to make into a list.</param>
        /// <returns>The items appended.</returns>
        public static object ListStar(object args)
        {
            return Rest(args) == List.Empty ? 
                First(args) : 
                Cons(First(args), ListStar(Rest(args)));
        }

        /// <summary>
        /// Get the first member of a list.
        /// </summary>
        /// <param name="x">The list to use.</param>
        /// <returns>The first member of the list, or the empty list if not a list.</returns>
        public static object First(object x)
        {
            return x is Pair ? ((Pair)x).FirstCell : List.Empty;
        }

        /// <summary>
        /// Get the second member of a list.
        /// </summary>
        /// <param name="x">The list to use.</param>
        /// <returns>The second member of the list, 
        /// or the empty list if there is none.</returns>
        public static object Second(object x)
        {
            return First(Rest(x));
        }

        /// <summary>
        /// Get the third member of a list.
        /// </summary>
        /// <param name="x">The list to use.</param>
        /// <returns>The third member of the list, or the empty list if there is none.</returns>
        public static object Third(object x)
        {
            return First(Rest(Rest(x)));
        }

        /// <summary>
        /// Return the rest of a list.
        /// </summary>
        /// <param name="x">The list to use.</param>
        /// <returns>The rest -- the list with the first stripped off, 
        /// or the empty list if there is none.</returns>
        public static object Rest(object x)
        {
            return x is Pair ? ((Pair)x).RestCell : List.Empty;
        }

        /// <summary>
        /// Construct a pair from two objects.
        /// </summary>
        /// <param name="a">The first object.</param>
        /// <param name="b">The rest of the objects.</param>
        /// <returns>The pair resulting from the construction.</returns>
        public static Pair Cons(object a, object b)
        {
            return new Pair(a, b);
        }

        /// <summary>
        /// Determine the length of a list.
        /// We don't use the iterator because it is slower.
        /// </summary>
        /// <param name="x">The list to measure.</param>
        /// <returns>The list length.</returns>
        public static int Length(object x)
        {
            int len = 0;
            while (x is Pair)
            {
                len++;
                x = ((Pair)x).RestCell;
            }

            return len;
        }

        /// <summary>
        /// Traverse the given list, applying the given function to all elements.
        /// Create a list of the results.
        /// This is purely iterative.
        /// </summary>
        /// <param name="fun">The function to apply to each elment.</param>
        /// <param name="expr">The list to process.</param>
        /// <returns>A list made up of the function results of each input element.  Could be the empty list.</returns>
        public static object MapFun(Func<object, object> fun, object expr)
        {
            Pair result = MakeEmptyList();
            Pair accum = result;

            // Iterate down the list, taking the function and building a list of the results.
            expr = First(expr);
            while (expr is Pair)
            {
                accum = (Pair)(accum.RestCell = MakeList(fun(First(expr))));
                expr = ((Pair)expr).RestCell;
            }

            return Rest(result);
        }
    }
}
