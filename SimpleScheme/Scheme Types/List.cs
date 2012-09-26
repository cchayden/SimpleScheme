// <copyright file="List.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using Obj = System.Object;

    /// <summary>
    /// List utilities used by the primitives and list primitives.
    /// </summary>
    public static class List
    {
        #region Public Methods
        /// <summary>
        /// Create an empty list.
        /// This is actually a Pair whose First is null.
        /// This is not the same as EmptyList.Instance.
        /// Used where we need a list to start with, the first cell is normally trimmed off later.
        /// </summary>
        /// <returns>A list whose first cell is null.</returns>
        public static Pair MakeList()
        {
            return new Pair();
        }

        /// <summary>
        /// Create a list from an obj.
        /// Makes a list of length 1.
        /// </summary>
        /// <param name="a">The object to put into the list.</param>
        /// <returns>The Pair making up the head of the list.</returns>
        public static Pair MakeList(this Obj a)
        {
            return new Pair(a);
        }

        /// <summary>
        /// Create a list from two objs.
        /// Makes a list of length 2.
        /// </summary>
        /// <param name="a">The first obj.</param>
        /// <param name="b">The second obj.</param>
        /// <returns>The Pair making up the head of the list.</returns>
        public static Pair MakeList(this Obj a, Obj b)
        {
            return new Pair(a, b.MakeList());
        }

        /// <summary>
        /// Create a list from three objs.
        /// Makes a list of length 3.
        /// </summary>
        /// <param name="a">The first obj.</param>
        /// <param name="b">The second obj.</param>
        /// <param name="c">The third obj.</param>
        /// <returns>The Pair making up the head of the list.</returns>
        public static Pair MakeList(this Obj a, Obj b, Obj c)
        {
            return a.MakeList(MakeList(b, c.MakeList()));
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
        public static Pair MakeList(this Obj a, Obj b, Obj c, Obj d)
        {
            return a.MakeList(MakeList(b, MakeList(c, d.MakeList())));
        }

        /// <summary>
        /// Construct a pair from two objs.
        /// </summary>
        /// <param name="a">The first obj.</param>
        /// <param name="b">The rest of the objs.</param>
        /// <returns>The pair resulting from the construction.</returns>
        public static Pair Cons(this Obj a, Obj b)
        {
            return new Pair(a, b);
        }

        /// <summary>
        /// Get the first member of a list.
        /// </summary>
        /// <param name="list">The list to use.</param>
        /// <returns>The first member of the list, or the empty list if not a list.</returns>
        public static Obj First(this Obj list)
        {
            return list.IsPair() ? list.AsPair().First : EmptyList.New();
        }

        /// <summary>
        /// Get the second member of a list.
        /// </summary>
        /// <param name="list">The list to use.</param>
        /// <returns>The second member of the list, 
        /// or the empty list if there is none.</returns>
        public static Obj Second(this Obj list)
        {
            return list.Rest().First();
        }

        /// <summary>
        /// Get the third member of a list.
        /// </summary>
        /// <param name="list">The list to use.</param>
        /// <returns>The third member of the list, or the empty list if there is none.</returns>
        public static Obj Third(this Obj list)
        {
            return list.Rest().Rest().First();
        }

        /// <summary>
        /// Return the rest of a list.
        /// </summary>
        /// <param name="list">The list to use.</param>
        /// <returns>The rest -- the list with the first stripped off, 
        /// or the empty list if there is none.</returns>
        public static Obj Rest(this Obj list)
        {
            return list.IsPair() ? list.AsPair().Rest : EmptyList.New();
        }

        /// <summary>
        /// Determine the length of a list.
        /// </summary>
        /// <param name="list">The list to measure.</param>
        /// <returns>The list length.</returns>
        public static int ListLength(this Obj list)
        {
            int len = 0;
            while (list.IsPair())
            {
                len++;
                list = list.Rest();
            }

            return len;
        }

        // Destructive list operations

        /// <summary>
        /// Set the First member of a pair destructively.
        /// </summary>
        /// <param name="pair">The pair whose First cell we want to modify.</param>
        /// <param name="newValue">The new value to put into it.</param>
        /// <returns>Undefined instance.</returns>
        public static Obj SetFirst(this Obj pair, Obj newValue)
        {
            if (pair.IsPair())
            {
                pair.AsPair().SetFirst(newValue);
                return Undefined.New();
            }

            return ErrorHandlers.SemanticError(string.Format(@"Attempt to set-car! of a non-Pair: ""{0}""", Printer.AsString(pair)));
        }

        /// <summary>
        /// Set the Rest member of a pair destructively.
        /// </summary>
        /// <param name="pair">The pair whose Rest cell we want to modify.</param>
        /// <param name="newTail">The new value to put into it.</param>
        /// <returns>Undefined instance.</returns>
        public static Obj SetRest(this Obj pair, Obj newTail)
        {
            if (pair.IsPair())
            {
                pair.AsPair().SetRest(newTail);
                return Undefined.New();
            }

            return ErrorHandlers.SemanticError(string.Format(@"Attempt to set-cdr! of a non-Pair: ""{0}""", Printer.AsString(pair)));
        }

        /// <summary>
        /// Create a list made out of all the objs given.
        /// This turns an improper list into a proper list by moving the improper
        ///   element into its own list cell.
        /// This is used to prepare the arguments for "apply".
        /// </summary>
        /// <param name="args">The obj to make into a list.</param>
        /// <returns>The items appended.</returns>
        public static Obj ListStar(this Obj args)
        {
            // only one arg -- take it
            if (args.Rest().IsEmptyList())
            {
                return args.First();
            }

            // More than one arg:
            // Copy all but last into a list.
            // Then splice in the last.
            Pair result = MakeList();
            Pair accum = result;
            Obj expr = args;

            // Iterate down the list, building a list of the results.
            while (!expr.Rest().IsEmptyList())
            {
                accum = accum.SetRest(expr.First().MakeList()).AsPair();
                expr = expr.Rest();
            }

            // Splice on the last arg
            // First, check that the last arg is a list.
            Obj rest = expr.First();
            if (!rest.IsPair())
            {
                ErrorHandlers.Error("ListStar: last argument is not a list: " + args);
            }

            accum.SetRest(rest);
            return result.Rest();
        }

        /// <summary>
        /// Create a list containing objs in the given list in the reverse order.
        /// </summary>
        /// <param name="x">The list to reverse.</param>
        /// <returns>The reversed list.</returns>
        public static Obj ReverseList(this Obj x)
        {
            Obj result = EmptyList.New();
            while (x.IsPair())
            {
                result = x.First().Cons(result);
                x = x.Rest();
            }

            return result;
        }

        /// <summary>
        /// Create a new list of the given length, where each element is given.
        /// </summary>
        /// <param name="n">The length of the list.</param>
        /// <param name="fill">The element to place in each list element.</param>
        /// <returns>A list filled with the given element.</returns>
        public static Obj Fill(int n, Obj fill)
        {
            Obj res = EmptyList.New();
            for (int i = 0; i < n; i++)
            {
                res = fill.Cons(res);
            }

            return res;
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
            Pair result = MakeList();
            Pair accum = result;

            // Iterate down the list, taking the function and building a list of the results.
            expr = expr.First();
            while (expr.IsPair())
            {
                accum = accum.SetRest(fun(expr.First()).MakeList()).AsPair();
                expr = expr.Rest();
            }

            return result.Rest();
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the list primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(PrimitiveEnvironment env)
        {
            const int MaxInt = int.MaxValue;
            env
                //// <r4rs section="6.3">(append <list> ...)</r4rs>
                .DefinePrimitive(
                    Symbol.New("append"), 
                    (args, caller) => Append(args), 
                    0, 
                    MaxInt, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.3">(assoc <obj> <alist>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("assoc"), 
                    (args, caller) => MemberAssoc(args.First(), args.Second(), x => x.First(), (x, y) => SchemeBoolean.Equal(x, y).Value), 
                    2,
                    TypePrimitives.ValueType.Obj, 
                    TypePrimitives.ValueType.Pair)
                //// <r4rs section="6.3">(assq <obj> <alist>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("assq"), 
                    (args, caller) => MemberAssoc(args.First(), args.Second(), x => x.First(), (x, y) => SchemeBoolean.Eqv(x, y).Value), 
                    2, 
                    TypePrimitives.ValueType.Obj, 
                    TypePrimitives.ValueType.Pair)
                //// <r4rs section="6.3">(assv <obj> <alist>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("assv"), 
                    (args, caller) => MemberAssoc(args.First(), args.Second(), x => x.First(), (x, y) => SchemeBoolean.Eqv(x, y).Value), 
                    2, 
                    TypePrimitives.ValueType.Obj, 
                    TypePrimitives.ValueType.Pair)
                //// <r4rs section="6.3">(car <pair>)</r4rs>
                .DefinePrimitive(
                     Symbol.New("car"), 
                     (args, caller) => args.First().First(), 
                     1, 
                     TypePrimitives.ValueType.Pair)
                //// (first <pair>)
                .DefinePrimitive(
                    Symbol.New("first"), 
                    (args, caller) => args.First().First(), 
                    1, 
                    TypePrimitives.ValueType.Pair)
                //// (second <pair>)
                .DefinePrimitive(
                    Symbol.New("second"), 
                    (args, caller) => args.First().Second(), 
                    1, 
                    TypePrimitives.ValueType.Pair)
                //// (third <pair>)
                .DefinePrimitive(
                    Symbol.New("third"), 
                    (args, caller) => args.First().Third(), 
                    1, 
                    TypePrimitives.ValueType.Pair)
                //// <r4rs section="6.3">(cdr <pair>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("cdr"), 
                    (args, caller) => args.First().Rest(),
                    1, 
                    TypePrimitives.ValueType.Pair)
                //// (rest <pair>)
                .DefinePrimitive(
                    Symbol.New("rest"), 
                    (args, caller) => args.First().Rest(), 
                    1, 
                    TypePrimitives.ValueType.Pair)
                //// <r4rs section="6.3">(cons <obj1> <obj2>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("cons"), 
                    (args, caller) => args.First().Cons(args.Second()), 
                    2, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.3">(length <list> ...)</r4rs>
                .DefinePrimitive(
                    Symbol.New("length"), 
                    (args, caller) => Number.New(args.First().ListLength()), 
                    1, 
                    TypePrimitives.ValueType.PairOrEmpty)
                //// <r4rs section="6.3">(list <obj> ...)</r4rs>
                .DefinePrimitive(
                    Symbol.New("list"), 
                    (args, caller) => args, 
                    0, 
                    MaxInt, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.3">(list-ref <list> <k>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("list-ref"), 
                    (args, caller) => ListRef(args.First(), args.Second()), 
                    2, 
                    TypePrimitives.ValueType.Pair, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.3">(list-tail <list> <k>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("list-tail"), 
                    (args, caller) => ListTail(args.First(), args.Second()), 
                    2, 
                    TypePrimitives.ValueType.Pair, 
                    TypePrimitives.ValueType.Number)
                .DefinePrimitive(
                    Symbol.New("list*"),
                    (args, caller) => args.ListStar(), 
                    2, 
                    MaxInt, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.3">(list? <obj>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("list?"), 
                    (args, caller) => SchemeBoolean.Truth(IsList(args.First())), 
                    1, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.3">(member <obj> <list>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("member"), 
                    (args, caller) => MemberAssoc(args.First(), Second(args), x => x, (x, y) => SchemeBoolean.Equal(x, y).Value), 
                    2,
                    TypePrimitives.ValueType.Obj, 
                    TypePrimitives.ValueType.Pair)
                //// <r4rs section="6.3">(memq <obj> <list>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("memq"), 
                    (args, caller) => MemberAssoc(args.First(), args.Second(), x => x, (x, y) => SchemeBoolean.Eqv(x, y).Value), 
                    2,
                    TypePrimitives.ValueType.Obj, 
                    TypePrimitives.ValueType.Pair)
                //// <r4rs section="6.3">(memv <obj> <list>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("memv"), 
                    (args, caller) => MemberAssoc(args.First(), args.Second(), x => x, (x, y) => SchemeBoolean.Eqv(x, y).Value), 
                    2,
                    TypePrimitives.ValueType.Obj, 
                    TypePrimitives.ValueType.Pair)
                //// <r4rs section="6.3">(pair? <obj>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("pair?"), 
                    (args, caller) => SchemeBoolean.Truth(args.First().IsPair()), 
                    1, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.3">(reverse <list>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("reverse"), 
                    (args, caller) => args.First().ReverseList(), 
                    1, 
                    TypePrimitives.ValueType.Pair)
                //// <r4rs section="6.3">(set-car! <pair> <obj>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("set-car!"), 
                    (args, caller) => args.First().SetFirst(args.Second()), 
                    2, 
                    TypePrimitives.ValueType.Pair, 
                    TypePrimitives.ValueType.Obj)
                //// (set-first! <pair> <obj>)
                .DefinePrimitive(
                    Symbol.New("set-first!"), 
                    (args, caller) => args.First().SetFirst(args.Second()), 
                    2, 
                    TypePrimitives.ValueType.Pair, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.3">(set-cdr! <pair> <obj>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("set-cdr!"), 
                    (args, caller) => args.First().SetRest(args.Second()), 
                    2, 
                    TypePrimitives.ValueType.Pair, 
                    TypePrimitives.ValueType.Obj)
                //// (set-rest! <pair> <obj>)
                .DefinePrimitive(
                    Symbol.New("set-rest!"), 
                    (args, caller) => args.First().SetRest(args.Second()), 
                    2, 
                    TypePrimitives.ValueType.Pair, 
                    TypePrimitives.ValueType.Obj);

            DefineAccessPrimitives(env, "aa");
            DefineAccessPrimitives(env, "ad");
            DefineAccessPrimitives(env, "da");
            DefineAccessPrimitives(env, "dd");
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Do all the combination car-cdr functions.
        /// </summary>
        /// <param name="name">The function name.</param>
        /// <param name="args">The expression to operate on.</param>
        /// <returns>The result of the operation.</returns>
        private static Obj Cxr(string name, Obj args)
        {
            Obj first = args.First();
            for (int i = name.Length - 2; i >= 1; i--)
            {
                first = name[i] == 'a' ? first.First() : first.Rest();
            }

            return first;
        }

        /// <summary>
        /// Define all the combinations of c([ad]+)r functions up to four levels.
        /// </summary>
        /// <param name="env">The environment to define the functions in.</param>
        /// <param name="access">The access string so far.</param>
        private static void DefineAccessPrimitives(PrimitiveEnvironment env, string access)
        {
            string prim = "c" + access + "r";
            env.DefinePrimitive(Symbol.New(prim), (args, caller) => Cxr(prim, args), 1, TypePrimitives.ValueType.Pair);
            if (access.Length >= 4)
            {
                return;
            }

            DefineAccessPrimitives(env, access + "a");
            DefineAccessPrimitives(env, access + "d");
        }

        /// <summary>
        /// Append a list of lists, making one longer list.
        /// The appending only goes one level deep.
        /// The very last list is not copied, but is instead shared.
        /// </summary>
        /// <param name="args">A list of lists.  Each of the lists in this list is appended together.</param>
        /// <returns>A list of the given list elements.</returns>
        private static Obj Append(Obj args)
        {
            if (args.IsEmptyList())
            {
                return EmptyList.New();
            }

            Pair result = MakeList();
            Pair accum = result;

            while (!args.Rest().IsEmptyList())
            {
                accum = Append(accum, args.First());
                args = args.Rest();
            }

            accum.SetRest(args.First());

            return result.Rest();
        }

        /// <summary>
        /// Append one list to the tail of another.
        /// The tail is modified destructively.
        /// The appended list is copied.
        /// </summary>
        /// <param name="tail">The end of the first list, destructively appended to.</param>
        /// <param name="toCopy">The second list, copied onto the first.</param>
        /// <returns>The end of the second list, suitable for another call to this function. </returns>
        private static Pair Append(Pair tail, Obj toCopy)
        {
            while (!toCopy.IsEmptyList())
            {
                tail.SetRest(toCopy.First().MakeList());
                toCopy = toCopy.Rest();
                tail = tail.Rest().AsPair();
            }

            return tail;
        }

        /// <summary>
        /// Tests to see if the given obj is a list.
        /// Tests for short loops, but if there is a bigger loop, this will hang.
        /// </summary>
        /// <param name="x">The obj to test.</param>
        /// <returns>True if the obj is a list.</returns>
        private static bool IsList(Obj x)
        {
            while (true)
            {
                if (x.IsEmptyList())
                {
                    return true;
                }

                if (!x.IsPair())
                {
                    return false;
                }

                Obj rest = x.Rest();
                if (rest == x)
                {
                    return false;
                }

                x = rest;
            }
        }

        /// <summary>
        /// Find the k-th element of the list.
        /// </summary>
        /// <param name="list">The starting list.</param>
        /// <param name="k">The number of tails to take.</param>
        /// <returns>The element after stepping down k steps.</returns>
        private static Obj ListRef(Obj list, Obj k)
        {
            for (int i = k.AsInt(); i > 0; i--)
            {
                list = list.Rest();
            }

            return list.First();
        }

        /// <summary>
        /// Take the k-th tail of the list.
        /// </summary>
        /// <param name="list">The starting list.</param>
        /// <param name="k">The number of tails to take.</param>
        /// <returns>The list after stepping down k steps.</returns>
        private static Obj ListTail(Obj list, Obj k)
        {
            for (int i = k.AsInt(); i > 0; i--)
            {
                list = list.Rest();
            }

            return list;
        }

        /// <summary>
        /// Searches lists, used by memq, memv, and member.
        /// Also used by assq, assv, and assoc.
        /// </summary>
        /// <param name="obj">The obj to search for.</param>
        /// <param name="list">The list to search in.</param>
        /// <param name="sel">Select the target and return method.</param>
        /// <param name="eq">This gives the type of equality test to use.</param>
        /// <returns>The results that were found.</returns>
        private static Obj MemberAssoc(Obj obj, Obj list, Func<Obj, Obj> sel, Func<Obj, Obj, bool> eq)
        {
            while (list.IsPair())
            {
                if (eq(sel(list.First()), obj))
                {
                    return sel(list);
                }

                list = list.Rest();
            }

            return SchemeBoolean.False;
        }
        #endregion
    }
}
