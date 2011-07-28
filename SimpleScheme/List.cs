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
        public static Pair New()
        {
            return new Pair();
        }

        /// <summary>
        /// Create a list from an obj.
        /// Makes a list of length 1.
        /// </summary>
        /// <param name="a">The object to put into the list.</param>
        /// <returns>The Pair making up the head of the list.</returns>
        public static Pair New(Obj a)
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
        public static Pair New(Obj a, Obj b)
        {
            return new Pair(a, new Pair(b));
        }

        /// <summary>
        /// Create a list from three objs.
        /// Makes a list of length 3.
        /// </summary>
        /// <param name="a">The first obj.</param>
        /// <param name="b">The second obj.</param>
        /// <param name="c">The third obj.</param>
        /// <returns>The Pair making up the head of the list.</returns>
        public static Pair New(Obj a, Obj b, Obj c)
        {
            return new Pair(a, new Pair(b, new Pair(c)));
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
        public static Pair New(Obj a, Obj b, Obj c, Obj d)
        {
            return new Pair(a, new Pair(b, new Pair(c, new Pair(d))));
        }

        /// <summary>
        /// Construct a pair from two objs.
        /// </summary>
        /// <param name="a">The first obj.</param>
        /// <param name="b">The rest of the objs.</param>
        /// <returns>The pair resulting from the construction.</returns>
        public static Pair Cons(Obj a, Obj b)
        {
            return new Pair(a, b);
        }

        /// <summary>
        /// Determine the length of a list.
        /// </summary>
        /// <param name="list">The list to measure.</param>
        /// <returns>The list length.</returns>
        public static int Length(Obj list)
        {
            int len = 0;
            while (Pair.IsPair(list))
            {
                len++;
                list = Rest(list);
            }

            return len;
        }

        /// <summary>
        /// Get the first member of a list.
        /// </summary>
        /// <param name="list">The list to use.</param>
        /// <returns>The first member of the list, or the empty list if not a list.</returns>
        public static Obj First(Obj list)
        {
            return Pair.IsPair(list) ? ((Pair)list).First : EmptyList.Instance;
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
            return Pair.IsPair(list) ? ((Pair)list).Rest : EmptyList.Instance;
        }

        /// <summary>
        /// Create a list made out of all the objs given.
        /// This turns an improper list into a proper list by moving the improper
        ///   element into its own list cell.
        /// This is used to prepare the arguments for "apply".
        /// </summary>
        /// <param name="args">The obj to make into a list.</param>
        /// <returns>The items appended.</returns>
        public static Obj ListStar(Obj args)
        {
            // only one arg -- take it
            if (EmptyList.IsEmptyList(Rest(args)))
            {
                return First(args);
            }

            // More than one arg:
            // Copy all but last into a list.
            // Then splice in the last.
            Pair result = New();
            Pair accum = result;
            Obj expr = args;

            // Iterate down the list, building a list of the results.
            while (!EmptyList.IsEmptyList(Rest(expr)))
            {
                accum = (Pair)(accum.Rest = New(First(expr)));
                expr = Rest(expr);
            }

            // Splice on the last arg
            // First, check that the last arg is a list.
            Obj rest = First(expr);
            if (!Pair.IsPair(rest))
            {
                ErrorHandlers.Error("ListStar: last argument is not a list: " + args);
            }

            accum.Rest = rest;
            return Rest(result);
        }

        /// <summary>
        /// Create a list containing objs in the given list in the reverse order.
        /// </summary>
        /// <param name="x">The list to reverse.</param>
        /// <returns>The reversed list.</returns>
        public static Obj Reverse(Obj x)
        {
            Obj result = EmptyList.Instance;
            while (Pair.IsPair(x))
            {
                result = Cons(First(x), result);
                x = Rest(x);
            }

            return result;
        }

        /// <summary>
        /// Traverse the given list, applying the given function to all elements.
        /// Create a list of the results.
        /// This is purely iterative.
        /// </summary>
        /// <param name="fun">The function to apply to each elment.</param>
        /// <param name="expr">The list to process.</param>
        /// <returns>A list made up of the function results of each input element.  Could be the empty list.</returns>
        public static Obj MapFun(Func<object, object> fun, Obj expr)
        {
            Pair result = New();
            Pair accum = result;

            // Iterate down the list, taking the function and building a list of the results.
            expr = First(expr);
            while (Pair.IsPair(expr))
            {
                accum = (Pair)(accum.Rest = New(fun(First(expr))));
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
            const int MaxInt = Int32.MaxValue;
            env
                //// <r4rs section="6.3">(append <list> ...)</r4rs>
                .DefinePrimitive("append", (args, caller) => Append(args), 0, MaxInt)
                //// <r4rs section="6.3">(assoc <obj> <alist>)</r4rs>
                .DefinePrimitive("assoc", (args, caller) => MemberAssoc(First(args), Second(args), 'a', ' '), 2)
                //// <r4rs section="6.3">(assq <obj> <alist>)</r4rs>
                .DefinePrimitive("assq", (args, caller) => MemberAssoc(First(args), Second(args), 'a', 'q'), 2)
                //// <r4rs section="6.3">(assv <obj> <alist>)</r4rs>
                .DefinePrimitive("assv", (args, caller) => MemberAssoc(First(args), Second(args), 'a', 'v'), 2)
                //// <r4rs section="6.3">(car <pair>)</r4rs>
                .DefinePrimitive("car", (args, caller) => First(First(args)), 1)
                //// (first <pair>)
                .DefinePrimitive("first", (args, caller) => First(First(args)), 1)
                //// (second <pair>)
                .DefinePrimitive("second", (args, caller) => Second(First(args)), 1)
                //// (third <pair>)
                .DefinePrimitive("third", (args, caller) => Third(First(args)), 1)
                //// <r4rs section="6.3">(cdr <pair>)</r4rs>
                .DefinePrimitive("cdr", (args, caller) => Rest(First(args)), 1)
                //// (rest <pair>)
                .DefinePrimitive("rest", (args, caller) => Rest(First(args)), 1)
                //// <r4rs section="6.3">(cons <obj1> <obj2>)</r4rs>
                .DefinePrimitive("cons", (args, caller) => Cons(First(args), Second(args)), 2)
                //// <r4rs section="6.3">(length <list> ...)</r4rs>
                .DefinePrimitive("length", (args, caller) => Number.Num(Length(First(args))), 1)
                //// <r4rs section="6.3">(list <obj> ...)</r4rs>
                .DefinePrimitive("list", (args, caller) => args, 0, MaxInt)
                //// <r4rs section="6.3">(list-ref <list> <k>)</r4rs>
                .DefinePrimitive("list-ref", (args, caller) => ListRef(First(args), Second(args)), 2)
                //// <r4rs section="6.3">(list-tail <list> <k>)</r4rs>
                .DefinePrimitive("list-tail", (args, caller) => ListTail(First(args), Second(args)), 2)
                .DefinePrimitive("list*", (args, caller) => ListStar(args), 2, MaxInt)
                //// <r4rs section="6.3">(list? <obj>)</r4rs>
                .DefinePrimitive("list?", (args, caller) => SchemeBoolean.Truth(IsList(First(args))), 1)
                //// <r4rs section="6.3">(member <obj> <list>)</r4rs>
                .DefinePrimitive("member", (args, caller) => MemberAssoc(First(args), Second(args), 'm', ' '), 2)
                //// <r4rs section="6.3">(memq <obj> <list>)</r4rs>
                .DefinePrimitive("memq", (args, caller) => MemberAssoc(First(args), Second(args), 'm', 'q'), 2)
                //// <r4rs section="6.3">(memv <obj> <list>)</r4rs>
                .DefinePrimitive("memv", (args, caller) => MemberAssoc(First(args), Second(args), 'm', 'v'), 2)
                //// <r4rs section="6.3">(pair? <obj>)</r4rs>
                .DefinePrimitive("pair?", (args, caller) => SchemeBoolean.Truth(Pair.IsPair(First(args))), 1)
                //// <r4rs section="6.3">(reverse <list>)</r4rs>
                .DefinePrimitive("reverse", (args, caller) => Reverse(First(args)), 1)
                //// <r4rs section="6.3">(set-car! <pair> <obj>)</r4rs>
                .DefinePrimitive("set-car!", (args, caller) => SetFirst(First(args), Second(args)), 2)
                //// (set-first! <pair> <obj>)
                .DefinePrimitive("set-first!", (args, caller) => SetFirst(First(args), Second(args)), 2)
                //// <r4rs section="6.3">(set-cdr! <pair> <obj>)</r4rs>
                .DefinePrimitive("set-cdr!", (args, caller) => SetRest(First(args), Second(args)), 2)
                //// (set-rest! <pair> <obj>)
                .DefinePrimitive("set-rest!", (args, caller) => SetRest(First(args), Second(args)), 2);

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
            Obj first = First(args);
            for (int i = name.Length - 2; i >= 1; i--)
            {
                first = name[i] == 'a' ? First(first) : Rest(first);
            }

            return first;
        }

        /// <summary>
        /// Define all the combinations of c[ad]+r functions up to four levels.
        /// </summary>
        /// <param name="env">The environment to define the functions in.</param>
        /// <param name="access">The access string so far.</param>
        private static void DefineAccessPrimitives(PrimitiveEnvironment env, string access)
        {
            string prim = "c" + access + "r";
            env.DefinePrimitive(prim, (args, caller) => Cxr(prim, args), 1);
            if (access.Length >= 4)
            {
                return;
            }

            DefineAccessPrimitives(env, access + "a");
            DefineAccessPrimitives(env, access + "d");
        }

        // Destructive list operations

        /// <summary>
        /// Set the first member of a pair destructively.
        /// </summary>
        /// <param name="x">The pair whose first member we want to modify.</param>
        /// <param name="y">The new value to put into it.</param>
        /// <returns>The obj that has just been modified.</returns>
        private static Obj SetFirst(Obj x, Obj y)
        {
            if (Pair.IsPair(x))
            {
                ((Pair)x).First = y;
                return Undefined.Instance;
            }

            return ErrorHandlers.SemanticError("Attempt to set-car! of a non-Pair: " + Printer.AsString(x));
        }

        /// <summary>
        /// Set the second member of a pair (the rest) destructively.
        /// </summary>
        /// <param name="x">The pair whose second member we want to modify.</param>
        /// <param name="y">The new value to put into it.</param>
        /// <returns>The obj that has just been modified.</returns>
        private static Obj SetRest(Obj x, Obj y)
        {
            if (Pair.IsPair(x))
            {
                ((Pair)x).Rest = y;
                return Undefined.Instance;
            }

            return ErrorHandlers.SemanticError("Attempt to set-cdr! of a non-Pair: " + Printer.AsString(x));
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
            if (EmptyList.IsEmptyList(args))
            {
                return EmptyList.Instance;
            }

            Pair result = New();
            Pair accum = result;

            while (!EmptyList.IsEmptyList(Rest(args)))
            {
                accum = Append(accum, First(args));
                args = Rest(args);
            }

            accum.Rest = First(args);

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
        private static Pair Append(Pair tail, Obj toCopy)
        {
            while (!EmptyList.IsEmptyList(toCopy))
            {
                tail.Rest = New(First(toCopy));
                toCopy = Rest(toCopy);
                tail = (Pair)Rest(tail);
            }

            return tail;
        }

        /// <summary>
        /// Tests to see if the given obj is a list.
        /// </summary>
        /// <param name="x">The obj to test.</param>
        /// <returns>True if the obj is a list.</returns>
        private static bool IsList(Obj x)
        {
            while (true)
            {
                if (EmptyList.IsEmptyList(x))
                {
                    return true;
                }

                if (!Pair.IsPair(x))
                {
                    return false;
                }

                Obj rest = Rest(x);
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
            for (int i = (int)Number.Num(k); i > 0; i--)
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
        private static Obj ListTail(Obj list, Obj k)
        {
            for (int i = (int)Number.Num(k); i > 0; i--)
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
        /// <param name="m">If 'm', do member, if 'a' do assoc.</param>
        /// <param name="eq">This gives the type of equality test to use.</param>
        /// <returns>The results that wer found.</returns>
        private static Obj MemberAssoc(Obj obj, Obj list, char m, char eq)
        {
            while (Pair.IsPair(list))
            {
                Obj target = m == 'm' ? First(list) : First(First(list));
                bool found;
                switch (eq)
                {
                    case 'q':
                        found = target == obj;
                        break;

                    case 'v':
                        found = SchemeBoolean.Eqv(target, obj);
                        break;

                    case ' ':
                        found = SchemeBoolean.Equal(target, obj);
                        break;

                    default:
                        ErrorHandlers.Warn("Internal error: bad option to MemberAssoc: " + eq);
                        return SchemeBoolean.False;
                }

                if (found)
                {
                    return m == 'm' ? list : First(list);
                }

                list = Rest(list);
            }

            return SchemeBoolean.False;
        }
        #endregion
    }
}
