// <copyright file="List.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

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
            return Cons(EmptyList.Instance, EmptyList.Instance);
        }

        /// <summary>
        /// Create a list from an obj.
        /// Makes a list of length 1.
        /// </summary>
        /// <param name="a">The object to put into the list.</param>
        /// <returns>The Pair making up the head of the list.</returns>
        public static Pair MakeList(ISchemeObject a)
        {
            return Cons(a, EmptyList.Instance);
        }

        /// <summary>
        /// Create a list from two objs.
        /// Makes a list of length 2.
        /// </summary>
        /// <param name="a">The first obj.</param>
        /// <param name="b">The second obj.</param>
        /// <returns>The Pair making up the head of the list.</returns>
        public static Pair MakeList(ISchemeObject a, ISchemeObject b)
        {
            return Cons(a, Pair.New(b));
        }

        /// <summary>
        /// Create a list from three objs.
        /// Makes a list of length 3.
        /// </summary>
        /// <param name="a">The first obj.</param>
        /// <param name="b">The second obj.</param>
        /// <param name="c">The third obj.</param>
        /// <returns>The Pair making up the head of the list.</returns>
        public static Pair MakeList(ISchemeObject a, ISchemeObject b, ISchemeObject c)
        {
            return Cons(a, Cons(b, Pair.New(c)));
        }

        /// <summary>
        /// Construct a pair from two objs.
        /// </summary>
        /// <param name="a">The first obj.</param>
        /// <param name="b">The rest of the objs.</param>
        /// <returns>The pair resulting from the construction.</returns>
        public static Pair Cons(ISchemeObject a, ISchemeObject b)
        {
            return Pair.New(a, b);
        }

        /// <summary>
        /// Get the first member of a list.
        /// </summary>
        /// <param name="list">The list to use.</param>
        /// <returns>The first member of the list, or the empty list if not a list.</returns>
        public static ISchemeObject First(ISchemeObject list)
        {
            return list is Pair ? list.AsPair().First : EmptyList.Instance;
        }

        /// <summary>
        /// Get the second member of a list.
        /// </summary>
        /// <param name="list">The list to use.</param>
        /// <returns>The second member of the list, 
        /// or the empty list if there is none.</returns>
        public static ISchemeObject Second(ISchemeObject list)
        {
            return First(Rest(list));
        }

        /// <summary>
        /// Get the third member of a list.
        /// </summary>
        /// <param name="list">The list to use.</param>
        /// <returns>The third member of the list, or the empty list if there is none.</returns>
        public static ISchemeObject Third(ISchemeObject list)
        {
            return First(Rest(Rest(list)));
        }

        /// <summary>
        /// Return the rest of a list.
        /// </summary>
        /// <param name="list">The list to use.</param>
        /// <returns>The rest -- the list with the first stripped off, 
        /// or the empty list if there is none.</returns>
        public static ISchemeObject Rest(ISchemeObject list)
        {
            return list is Pair ? list.AsPair().Rest : EmptyList.Instance;
        }

        /// <summary>
        /// Determine the length of a list.
        /// </summary>
        /// <param name="list">The list to measure.</param>
        /// <returns>The list length.</returns>
        public static int ListLength(ISchemeObject list)
        {
            int len = 0;
            while (list is Pair)
            {
                len++;
                list = Rest(list);
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
        public static ISchemeObject SetFirst(ISchemeObject pair, ISchemeObject newValue)
        {
            if (pair is Pair)
            {
                pair.AsPair().SetFirst(newValue);
                return Undefined.Instance;
            }

            return ErrorHandlers.SemanticError(string.Format(@"Attempt to set-car! of a non-Pair: ""{0}""", Printer.AsString(pair)));
        }

        /// <summary>
        /// Set the Rest member of a pair destructively.
        /// </summary>
        /// <param name="pair">The pair whose Rest cell we want to modify.</param>
        /// <param name="newTail">The new value to put into it.</param>
        /// <returns>Undefined instance.</returns>
        public static ISchemeObject SetRest(ISchemeObject pair, ISchemeObject newTail)
        {
            if (pair is Pair)
            {
                pair.AsPair().SetRest(newTail);
                return Undefined.Instance;
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
        public static ISchemeObject ListStar(ISchemeObject args)
        {
            // only one arg -- take it
            if (Rest(args) is EmptyList)
            {
                return First(args);
            }

            // More than one arg:
            // Copy all but last into a list.
            // Then splice in the last.
            Pair result = MakeList();
            Pair accum = result;
            ISchemeObject expr = args;

            // Iterate down the list, building a list of the results.
            while (!(Rest(expr) is EmptyList))
            {
                accum = accum.SetRest(MakeList(First(expr))).AsPair();
                expr = Rest(expr);
            }

            // Splice on the last arg
            // First, check that the last arg is a list.
            ISchemeObject rest = First(expr);
            if (!(rest is Pair))
            {
                ErrorHandlers.Error("ListStar: last argument is not a list: " + args);
            }

            accum.SetRest(rest);
            return Rest(result);
        }

        /// <summary>
        /// Create a list containing objs in the given list in the reverse order.
        /// </summary>
        /// <param name="x">The list to reverse.</param>
        /// <returns>The reversed list.</returns>
        public static ISchemeObject ReverseList(ISchemeObject x)
        {
            ISchemeObject result = EmptyList.Instance;
            while (x is Pair)
            {
                result = Cons(First(x), result);
                x = Rest(x);
            }

            return result;
        }

        /// <summary>
        /// Create a new list of the given length, where each element is given.
        /// </summary>
        /// <param name="n">The length of the list.</param>
        /// <param name="fill">The element to place in each list element.</param>
        /// <returns>A list filled with the given element.</returns>
        public static ISchemeObject Fill(int n, ISchemeObject fill)
        {
            ISchemeObject res = EmptyList.Instance;
            for (int i = 0; i < n; i++)
            {
                res = Cons(fill, res);
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
        public static ISchemeObject MapFun(Func<ISchemeObject, ISchemeObject> fun, ISchemeObject expr)
        {
            Pair result = MakeList();
            Pair accum = result;

            // Iterate down the list, taking the function and building a list of the results.
            expr = First(expr);
            while (expr is Pair)
            {
                accum = accum.SetRest(MakeList(fun(First(expr)))).AsPair();
                expr = Rest(expr);
            }

            return Rest(result);
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
                    "append",
                    (args, caller) => Append(args), 
                    0, 
                    MaxInt, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.3">(assoc <obj> <alist>)</r4rs>
                .DefinePrimitive(
                    "assoc", 
                    (args, caller) => MemberAssoc(First(args), Second(args), First, (x, y) => SchemeBoolean.Equal(x, y).Value), 
                    2,
                    TypePrimitives.ValueType.Obj, 
                    TypePrimitives.ValueType.Pair)
                //// <r4rs section="6.3">(assq <obj> <alist>)</r4rs>
                .DefinePrimitive(
                    "assq", 
                    (args, caller) => MemberAssoc(First(args), Second(args), First, (x, y) => SchemeBoolean.Eqv(x, y).Value), 
                    2, 
                    TypePrimitives.ValueType.Obj, 
                    TypePrimitives.ValueType.Pair)
                //// <r4rs section="6.3">(assv <obj> <alist>)</r4rs>
                .DefinePrimitive(
                    "assv", 
                    (args, caller) => MemberAssoc(First(args), Second(args), First, (x, y) => SchemeBoolean.Eqv(x, y).Value), 
                    2, 
                    TypePrimitives.ValueType.Obj, 
                    TypePrimitives.ValueType.Pair)
                //// <r4rs section="6.3">(car <pair>)</r4rs>
                .DefinePrimitive(
                     "car", 
                     (args, caller) => First(First(args)), 
                     1, 
                     TypePrimitives.ValueType.Pair)
                //// (first <pair>)
                .DefinePrimitive(
                    "first", 
                    (args, caller) => First(First(args)), 
                    1, 
                    TypePrimitives.ValueType.Pair)
                //// (second <pair>)
                .DefinePrimitive(
                    "second", 
                    (args, caller) => Second(First(args)), 
                    1, 
                    TypePrimitives.ValueType.Pair)
                //// (third <pair>)
                .DefinePrimitive(
                    "third", 
                    (args, caller) => Third(First(args)), 
                    1, 
                    TypePrimitives.ValueType.Pair)
                //// <r4rs section="6.3">(cdr <pair>)</r4rs>
                .DefinePrimitive(
                    "cdr", 
                    (args, caller) => Rest(First(args)),
                    1, 
                    TypePrimitives.ValueType.Pair)
                //// (rest <pair>)
                .DefinePrimitive(
                    "rest", 
                    (args, caller) => Rest(First(args)), 
                    1, 
                    TypePrimitives.ValueType.Pair)
                //// <r4rs section="6.3">(cons <obj1> <obj2>)</r4rs>
                .DefinePrimitive(
                    "cons", 
                    (args, caller) => Cons(First(args), Second(args)), 
                    2, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.3">(length <list> ...)</r4rs>
                .DefinePrimitive(
                    "length", 
                    (args, caller) => (Number)ListLength(First(args)),
                    1, 
                    TypePrimitives.ValueType.PairOrEmpty)
                //// <r4rs section="6.3">(list <obj> ...)</r4rs>
                .DefinePrimitive(
                    "list", 
                    (args, caller) => args, 
                    0, 
                    MaxInt, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.3">(list-ref <list> <k>)</r4rs>
                .DefinePrimitive(
                    "list-ref", 
                    (args, caller) => ListRef(First(args), Second(args)), 
                    2, 
                    TypePrimitives.ValueType.Pair, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.3">(list-tail <list> <k>)</r4rs>
                .DefinePrimitive(
                    "list-tail", 
                    (args, caller) => ListTail(First(args), Second(args)), 
                    2, 
                    TypePrimitives.ValueType.Pair, 
                    TypePrimitives.ValueType.Number)
                .DefinePrimitive(
                    "list*",
                    (args, caller) => ListStar(args), 
                    2, 
                    MaxInt, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.3">(list? <obj>)</r4rs>
                .DefinePrimitive(
                    "list?", 
                    (args, caller) => SchemeBoolean.Truth(IsList(First(args))), 
                    1, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.3">(member <obj> <list>)</r4rs>
                .DefinePrimitive(
                    "member", 
                    (args, caller) => MemberAssoc(First(args), Second(args), x => x, (x, y) => SchemeBoolean.Equal(x, y).Value), 
                    2,
                    TypePrimitives.ValueType.Obj, 
                    TypePrimitives.ValueType.Pair)
                //// <r4rs section="6.3">(memq <obj> <list>)</r4rs>
                .DefinePrimitive(
                    "memq", 
                    (args, caller) => MemberAssoc(First(args), Second(args), x => x, (x, y) => SchemeBoolean.Eqv(x, y).Value), 
                    2,
                    TypePrimitives.ValueType.Obj, 
                    TypePrimitives.ValueType.Pair)
                //// <r4rs section="6.3">(memv <obj> <list>)</r4rs>
                .DefinePrimitive(
                    "memv", 
                    (args, caller) => MemberAssoc(First(args), Second(args), x => x, (x, y) => SchemeBoolean.Eqv(x, y).Value), 
                    2,
                    TypePrimitives.ValueType.Obj, 
                    TypePrimitives.ValueType.Pair)
                //// <r4rs section="6.3">(pair? <obj>)</r4rs>
                .DefinePrimitive(
                    "pair?", 
                    (args, caller) => SchemeBoolean.Truth(First(args) is Pair), 
                    1, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.3">(reverse <list>)</r4rs>
                .DefinePrimitive(
                    "reverse", 
                    (args, caller) => ReverseList(First(args)), 
                    1, 
                    TypePrimitives.ValueType.Pair)
                //// <r4rs section="6.3">(set-car! <pair> <obj>)</r4rs>
                .DefinePrimitive(
                    "set-car!", 
                    (args, caller) => SetFirst(First(args), Second(args)), 
                    2, 
                    TypePrimitives.ValueType.Pair, 
                    TypePrimitives.ValueType.Obj)
                //// (set-first! <pair> <obj>)
                .DefinePrimitive(
                    "set-first!", 
                    (args, caller) => SetFirst(First(args), Second(args)), 
                    2, 
                    TypePrimitives.ValueType.Pair, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.3">(set-cdr! <pair> <obj>)</r4rs>
                .DefinePrimitive(
                    "set-cdr!", 
                    (args, caller) => SetRest(First(args), Second(args)), 
                    2, 
                    TypePrimitives.ValueType.Pair, 
                    TypePrimitives.ValueType.Obj)
                //// (set-rest! <pair> <obj>)
                .DefinePrimitive(
                    "set-rest!", 
                    (args, caller) => SetRest(First(args), Second(args)), 
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
        private static ISchemeObject Cxr(string name, ISchemeObject args)
        {
            ISchemeObject first = First(args);
            for (int i = name.Length - 2; i >= 1; i--)
            {
                first = name[i] == 'a' ? First(first) : Rest(first);
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
            env.DefinePrimitive(prim, (args, caller) => Cxr(prim, args), 1, TypePrimitives.ValueType.Pair);
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
        private static ISchemeObject Append(ISchemeObject args)
        {
            if (args is EmptyList)
            {
                return EmptyList.Instance;
            }

            Pair result = MakeList();
            Pair accum = result;

            while (!(Rest(args) is EmptyList))
            {
                accum = Append(accum, First(args));
                args = Rest(args);
            }

            accum.SetRest(First(args));

            return Rest(result);
        }

        /// <summary>
        /// Append one list to the tail of another.
        /// The tail is modified destructively.
        /// The appended list is copied.
        /// </summary>
        /// <param name="tail">The end of the first list, destructively appended to.</param>
        /// <param name="toCopy">The second list, copied onto the first.</param>
        /// <returns>The end of the second list, suitable for another call to this function. </returns>
        private static Pair Append(Pair tail, ISchemeObject toCopy)
        {
            while (!(toCopy is EmptyList))
            {
                tail.SetRest(MakeList(First(toCopy)));
                toCopy = Rest(toCopy);
                tail = Rest(tail).AsPair();
            }

            return tail;
        }

        /// <summary>
        /// Tests to see if the given obj is a list.
        /// Tests for short loops, but if there is a bigger loop, this will hang.
        /// </summary>
        /// <param name="x">The obj to test.</param>
        /// <returns>True if the obj is a list.</returns>
        private static bool IsList(ISchemeObject x)
        {
            while (true)
            {
                if (x is EmptyList)
                {
                    return true;
                }

                if (!(x is Pair))
                {
                    return false;
                }

                ISchemeObject rest = Rest(x);
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
        private static ISchemeObject ListRef(ISchemeObject list, ISchemeObject k)
        {
            for (int i = k.AsInt(); i > 0; i--)
            {
                list = Rest(list);
            }

            return First(list);
        }

        /// <summary>
        /// Take the k-th tail of the list.
        /// </summary>
        /// <param name="list">The starting list.</param>
        /// <param name="k">The number of tails to take.</param>
        /// <returns>The list after stepping down k steps.</returns>
        private static ISchemeObject ListTail(ISchemeObject list, ISchemeObject k)
        {
            for (int i = k.AsInt(); i > 0; i--)
            {
                list = Rest(list);
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
        private static ISchemeObject MemberAssoc(ISchemeObject obj, ISchemeObject list, Func<ISchemeObject, ISchemeObject> sel, Func<ISchemeObject, ISchemeObject, bool> eq)
        {
            while (list is Pair)
            {
                if (eq(sel(First(list)), obj))
                {
                    return sel(list);
                }

                list = Rest(list);
            }

            return (SchemeBoolean)false;
        }
        #endregion
    }
}
