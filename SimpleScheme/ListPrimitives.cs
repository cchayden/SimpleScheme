// <copyright file="ListPrimitives.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using Obj = System.Object;

    /// <summary>
    /// Common list primitives used widely.
    /// This is designed to serve as a base class for those that want to use
    ///   these functions without qualification.
    /// Because of this, it has no member variables.
    /// </summary>
    public class ListPrimitives
    {
        /// <summary>
        /// The signature for primitives.
        /// </summary>
        /// <param name="args">The primitive's arguments</param>
        /// <param name="caller">The calling stepper.</param>
        /// <returns>The primitive's result.</returns>
        public delegate Obj Op(Obj args, Stepper caller);

        #region Public Static Methods
        /// <summary>
        /// Create a list from an obj.
        /// Makes a list of length 1.
        /// </summary>
        /// <param name="a">The object to put into the list.</param>
        /// <returns>The Pair making up the head of the list.</returns>
        public static Pair MakeList(Obj a)
        {
            return Pair.New(a, EmptyList.Instance);
        }

        /// <summary>
        /// Create an empty list.
        /// This is actually a Pair whose First is null.
        /// This is not the same as EmptyList.Instance.
        /// Used where we need a list to start with, the first cell is normally trimmed off later.
        /// </summary>
        /// <returns>A list whose first cell is null.</returns>
        public static Pair MakeEmptyList()
        {
            return Pair.New(EmptyList.Instance, EmptyList.Instance);
        }

        /// <summary>
        /// Create a list from two objs.
        /// Makes a list of length 2.
        /// </summary>
        /// <param name="a">The first obj.</param>
        /// <param name="b">The second obj.</param>
        /// <returns>The Pair making up the head of the list.</returns>
        public static Pair MakeList(Obj a, Obj b)
        {
            return Pair.New(a, Pair.New(b, EmptyList.Instance));
        }

        /// <summary>
        /// Create a list from three objs.
        /// Makes a list of length 3.
        /// </summary>
        /// <param name="a">The first obj.</param>
        /// <param name="b">The second obj.</param>
        /// <param name="c">The third obj.</param>
        /// <returns>The Pair making up the head of the list.</returns>
        public static Pair MakeList(Obj a, Obj b, Obj c)
        {
            return Pair.New(a, Pair.New(b, Pair.New(c, EmptyList.Instance)));
        }

        /// <summary>
        /// Create a list from four objs.
        /// Makes a list of length 4.
        /// </summary>
        /// <param name="a">The first obj.</param>
        /// <param name="b">The second obj.</param>
        /// <param name="c">The third obj.</param>
        /// <param name="d">The fourth obj.</param>
        /// <returns>The Pair making up the head of the list.</returns>
        public static Pair MakeList(Obj a, Obj b, Obj c, Obj d)
        {
            return Pair.New(a, Pair.New(b, Pair.New(c, Pair.New(d, EmptyList.Instance))));
        }

        /// <summary>
        /// Create a list made out of all the objs given.
        /// </summary>
        /// <param name="args">The obj to make into a list.</param>
        /// <returns>The items appended.</returns>
        public static Obj ListStar(Obj args)
        {
            return EmptyList.IsType(Rest(args)) ? 
                First(args) : 
                Cons(First(args), ListStar(Rest(args)));
        }

        /// <summary>
        /// Get the first member of a list.
        /// </summary>
        /// <param name="list">The list to use.</param>
        /// <returns>The first member of the list, or the empty list if not a list.</returns>
        public static Obj First(Obj list)
        {
            return list is Pair ? ((Pair)list).FirstCell : EmptyList.Instance;
        }

        /// <summary>
        /// Get the second member of a list.
        /// </summary>
        /// <param name="list">The list to use.</param>
        /// <returns>The second member of the list, 
        /// or the empty list if there is none.</returns>
        public static Obj Second(Obj list)
        {
            return First(Rest(list));
        }

        /// <summary>
        /// Get the third member of a list.
        /// </summary>
        /// <param name="list">The list to use.</param>
        /// <returns>The third member of the list, or the empty list if there is none.</returns>
        public static Obj Third(Obj list)
        {
            return First(Rest(Rest(list)));
        }

        /// <summary>
        /// Return the rest of a list.
        /// </summary>
        /// <param name="list">The list to use.</param>
        /// <returns>The rest -- the list with the first stripped off, 
        /// or the empty list if there is none.</returns>
        public static Obj Rest(Obj list)
        {
            return list is Pair ? ((Pair)list).RestCell : EmptyList.Instance;
        }

        /// <summary>
        /// Construct a pair from two objs.
        /// </summary>
        /// <param name="a">The first obj.</param>
        /// <param name="b">The rest of the objs.</param>
        /// <returns>The pair resulting from the construction.</returns>
        public static Pair Cons(Obj a, Obj b)
        {
            return Pair.New(a, b);
        }

        /// <summary>
        /// Determine the length of a list.
        /// </summary>
        /// <param name="list">The list to measure.</param>
        /// <returns>The list length.</returns>
        public static int Length(Obj list)
        {
            int len = 0;
            while (list is Pair)
            {
                len++;
                list = ((Pair)list).RestCell;
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
        public static Obj MapFun(Func<Obj, Obj> fun, Obj expr)
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
        #endregion
    }
}
