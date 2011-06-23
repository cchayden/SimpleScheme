// <copyright file="List.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// List utilities used by the primitives.
    /// </summary>
    public sealed class List
    {
        /// <summary>
        /// Define the list primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(Environment env)
        {
            const int MaxInt = int.MaxValue;
            env
                .DefinePrimitive("append", (parent, args) => args == null ? null : Append(args), 0, MaxInt)
                .DefinePrimitive("assoc", (parent, args) => MemberAssoc(First(args), Second(args), 'a', ' '), 2)
                .DefinePrimitive("assq", (parent, args) => MemberAssoc(First(args), Second(args), 'a', 'q'), 2)
                .DefinePrimitive("assv", (parent, args) => MemberAssoc(First(args), Second(args), 'a', 'v'), 2)
                .DefinePrimitive("caaaar", (parent, args) => Primitive.Cxr("caaaar", args), 1)
                .DefinePrimitive("caaadr", (parent, args) => Primitive.Cxr("caaadr", args), 1)
                .DefinePrimitive("caaar", (parent, args) => Primitive.Cxr("caaar", args), 1)
                .DefinePrimitive("caadar", (parent, args) => Primitive.Cxr("caadar", args), 1)
                .DefinePrimitive("caaddr", (parent, args) => Primitive.Cxr("caaddr", args), 1)
                .DefinePrimitive("caar", (parent, args) => Primitive.Cxr("caar", args), 1)
                .DefinePrimitive("cadaar", (parent, args) => Primitive.Cxr("cadaar", args), 1)
                .DefinePrimitive("cadadr", (parent, args) => Primitive.Cxr("cadadr", args), 1)
                .DefinePrimitive("cadar", (parent, args) => Primitive.Cxr("cadar", args), 1)
                .DefinePrimitive("caddar", (parent, args) => Primitive.Cxr("caddar", args), 1)
                .DefinePrimitive("cadddr", (parent, args) => Primitive.Cxr("cadddr", args), 1)
                .DefinePrimitive("caddr", (parent, args) => Primitive.Cxr("caddr", args), 1)
                .DefinePrimitive("cadr", (parent, args) => Primitive.Cxr("cadr", args), 1)
                .DefinePrimitive("car", (parent, args) => First(First(args)), 1)
                .DefinePrimitive("first", (parent, args) => First(First(args)), 1)
                .DefinePrimitive("second", (parent, args) => Second(First(args)), 1)
                .DefinePrimitive("third", (parent, args) => Third(First(args)), 1)
                .DefinePrimitive("cdaaar,", (parent, args) => Primitive.Cxr("cdaaar", args), 1)
                .DefinePrimitive("cdaadr", (parent, args) => Primitive.Cxr("cdaadr", args), 1)
                .DefinePrimitive("cdaar", (parent, args) => Primitive.Cxr("cdaar", args), 1)
                .DefinePrimitive("cdadar", (parent, args) => Primitive.Cxr("cdadar", args), 1)
                .DefinePrimitive("cdaddr", (parent, args) => Primitive.Cxr("cdaddr", args), 1)
                .DefinePrimitive("cdadr", (parent, args) => Primitive.Cxr("cdadr", args), 1)
                .DefinePrimitive("cdar", (parent, args) => Primitive.Cxr("cdar", args), 1)
                .DefinePrimitive("cddaar", (parent, args) => Primitive.Cxr("cddaar", args), 1)
                .DefinePrimitive("cddadr", (parent, args) => Primitive.Cxr("cddadr", args), 1)
                .DefinePrimitive("cddar", (parent, args) => Primitive.Cxr("cddar", args), 1)
                .DefinePrimitive("cdddar", (parent, args) => Primitive.Cxr("cdddar", args), 1)
                .DefinePrimitive("cddddr", (parent, args) => Primitive.Cxr("cddddr", args), 1)
                .DefinePrimitive("cdddr", (parent, args) => Primitive.Cxr("cdddr", args), 1)
                .DefinePrimitive("cddr", (parent, args) => Primitive.Cxr("cddr", args), 1)
                .DefinePrimitive("cdr", (parent, args) => Rest(First(args)), 1)
                .DefinePrimitive("rest", (parent, args) => Rest(First(args)), 1)
                .DefinePrimitive("cons", (parent, args) => Cons(First(args), Second(args)), 2)
                .DefinePrimitive("length", (parent, args) => Number.Num(Length(First(args))), 1)
                .DefinePrimitive("list", (parent, args) => args, 0, MaxInt)
                .DefinePrimitive("list->string", (parent, args) => SchemeString.ListToString(First(args)), 1)
                .DefinePrimitive("list->vector", (parent, args) => new Vector(First(args)), 1)
                .DefinePrimitive(
                   "list-ref",
                   (parent, args) =>
                   {
                       object first = First(args);
                       object second = Second(args);
                       for (int k = (int)Number.Num(second); k > 0; k--)
                       {
                           first = Rest(first);
                       }

                       return First(first);
                   },
                    2)
                .DefinePrimitive(
                   "list-tail",
                   (parent, args) =>
                   {
                       object first = First(args);
                       object second = Second(args);
                       for (int k = (int)Number.Num(second); k > 0; k--)
                       {
                           first = Rest(first);
                       }

                       return first;
                   },
                    2)
                .DefinePrimitive("list?", (parent, args) => SchemeBoolean.Truth(IsList(First(args))), 1)
                .DefinePrimitive("member", (parent, args) => MemberAssoc(First(args), Second(args), 'm', ' '), 2)
                .DefinePrimitive("memq", (parent, args) => MemberAssoc(First(args), Second(args), 'm', 'q'), 2)
                .DefinePrimitive("memv", (parent, args) => MemberAssoc(First(args), Second(args), 'm', 'v'), 2)
                .DefinePrimitive("pair?", (parent, args) => SchemeBoolean.Truth(First(args) is Pair), 1)
                .DefinePrimitive("reverse", (parent, args) => Reverse(First(args)), 1)
                .DefinePrimitive("set-car!", (parent, args) => SetFirst(First(args), Second(args)), 2)
                .DefinePrimitive("set-first!", (parent, args) => SetFirst(First(args), Second(args)), 2)
                .DefinePrimitive("set-cdr!", (parent, args) => SetRest(First(args), Second(args)), 2)
                .DefinePrimitive("set-rest!", (parent, args) => SetRest(First(args), Second(args)), 2);
        }

        /// <summary>
        /// Create a list from an object.
        /// Makes a list of length 1.
        /// </summary>
        /// <param name="a">The object to put into the list.</param>
        /// <returns>The Pair making up the head of the list.</returns>
        public static Pair MakeList(object a)
        {
            return new Pair(a, null);
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
            return new Pair(a, new Pair(b, null));
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
            return new Pair(a, new Pair(b, new Pair(c, null)));
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
            return new Pair(a, new Pair(b, new Pair(c, new Pair(d, null))));
        }

        /// <summary>
        /// Create a list made out of all the objects given.
        /// </summary>
        /// <param name="args">The object to make into a list.</param>
        /// <returns>The items appended.</returns>
        public static object ListStar(object args)
        {
            return Rest(args) == null ? 
                First(args) : 
                Cons(First(args), ListStar(Rest(args)));
        }

        /// <summary>
        /// Get the first member of a list.
        /// </summary>
        /// <param name="x">The list to use.</param>
        /// <returns>The first member of the list, or null if not a list.</returns>
        public static object First(object x)
        {
            return x is Pair ? ((Pair)x).First : null;
        }

        /// <summary>
        /// Get the second member of a list.
        /// </summary>
        /// <param name="x">The list to use.</param>
        /// <returns>The second member of the list, 
        /// or null if there is none.</returns>
        public static object Second(object x)
        {
            return First(Rest(x));
        }

        /// <summary>
        /// Get the third member of a list.
        /// </summary>
        /// <param name="x">The list to use.</param>
        /// <returns>The third member of the list, or null if there is none.</returns>
        public static object Third(object x)
        {
            return First(Rest(Rest(x)));
        }

        /// <summary>
        /// Return the rest of a list.
        /// </summary>
        /// <param name="x">The list to use.</param>
        /// <returns>The rest -- the list with the first stripped off, 
        /// or null if there is none.</returns>
        public static object Rest(object x)
        {
            return x is Pair ? ((Pair)x).Rest : null;
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
        /// </summary>
        /// <param name="x">The list to measure.</param>
        /// <returns>The list length.</returns>
        public static int Length(object x)
        {
            int len = 0;
            if (x is Pair)
            {
                foreach (object elem in (Pair)x)
                {
                    len++;
                }
            }

            return len;
        }

        /// <summary>
        /// Traverse the given list, applying the given function to all elements.
        /// Create a list if the results.
        /// This is purely iterative.
        /// </summary>
        /// <param name="fun">The function to apply to each elment.</param>
        /// <param name="expr">The list to process.</param>
        /// <returns>A list made up of the function results of each input element.</returns>
        public static Pair MapFun(Func<object, object> fun, object expr)
        {
            Pair result = MakeList(null);
            Pair accum = result;

            // Iterate down the list, taking the function and building a list of the results.
            expr = First(expr);
            if (expr is Pair)
            {
                foreach (object elem in (Pair)expr)
                {
                    // Builds a list by tacking new values onto the tail.
                    accum = (Pair)(accum.Rest = MakeList(fun(elem)));
                }
            }

            return (Pair)Rest(result);
        }

        // Destructive list operations

        /// <summary>
        /// Set the first member of a pair destructively.
        /// </summary>
        /// <param name="x">The pair whose first member we want to modify.</param>
        /// <param name="y">The new value to put into it.</param>
        /// <returns>The object that has just been modified.</returns>
        private static object SetFirst(object x, object y)
        {
            return x is Pair ? 
                ((Pair)x).First = y : 
                ErrorHandlers.Error("SetFirst: attempt to set-car of a non-Pair: " + SchemeString.AsString(x));
        }

        /// <summary>
        /// Set the second member of a pair (the rest) destructively.
        /// </summary>
        /// <param name="x">The pair whose second member we want to modify.</param>
        /// <param name="y">The new value to put into it.</param>
        /// <returns>The object that has just been modified.</returns>
        private static object SetRest(object x, object y)
        {
            return x is Pair ? ((Pair)x).Rest = y : ErrorHandlers.Error("SetRest: attempt to set-cdr of a non-Pair: " + SchemeString.AsString(x));
        }

        /// <summary>
        /// Create a list containing objects in the given list in the reverse order.
        /// </summary>
        /// <param name="x">The list to reverse.</param>
        /// <returns>The reversed list.</returns>
        private static object Reverse(object x)
        {
            object result = null;
            if (x is Pair)
            {
                foreach (object elem in (Pair)x)
                {
                    result = Cons(elem, result);
                }
            }

            return result;
        }

        /// <summary>
        /// Append a list of lists, making one longer list.
        /// The appending only goes one level deep.
        /// The very last list is not copied, but is instead shared.
        /// </summary>
        /// <param name="args">A list of lists.  Each of the lists in this list is appended together.</param>
        /// <returns>A list of the given list elements.</returns>
        private static object Append(object args)
        {
            Pair result = MakeList(null);
            Pair accum = result;

            while (Rest(args) != null)
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
        private static Pair Append(Pair tail, object toCopy)
        {
            while (toCopy != null)
            {
                tail.Rest = MakeList(First(toCopy));
                toCopy = Rest(toCopy);
                tail = (Pair)Rest(tail);
            }

            return tail;
        }

        /// <summary>
        /// Tests to see if the given object is a list.
        /// </summary>
        /// <param name="x">The object to test.</param>
        /// <returns>True if the object is a list.</returns>
        private static bool IsList(object x)
        {
            while (true)
            {
                if (x == null)
                {
                    return true;
                }

                if (!(x is Pair))
                {
                    return false;
                }

                object rest = Rest(x);
                if (rest == x)
                {
                    return false;
                }

                x = rest;
            }
        }

        /// <summary>
        /// Searches lists, used by memq, memv, and member.
        /// Also used by assq, assv, and assoc.
        /// </summary>
        /// <param name="obj">The object to search for.</param>
        /// <param name="list">The list to search in.</param>
        /// <param name="m">If 'm', do member, if 'a' do assoc.</param>
        /// <param name="eq">This gives the type of equality test to use.</param>
        /// <returns>The results that wer found.</returns>
        private static object MemberAssoc(object obj, object list, char m, char eq)
        {
            // TODO convert to use foreach
            while (list is Pair)
            {
                object target = m == 'm' ? First(list) : First(First(list));
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
                        ErrorHandlers.Warn("Bad option to memberAssoc: " + eq);
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
    }
}
