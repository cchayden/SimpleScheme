// <copyright file="List.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// List utilities used by the primitives.
    /// </summary>
    public sealed class List : ListPrimitives
    {
        #region Define Primitives
        /// <summary>
        /// Define the list primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        internal static void DefinePrimitives(PrimitiveEnvironment env)
        {
            const int MaxInt = int.MaxValue;
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
                //// <r4rs section="6.3">(list? <obj>)</r4rs>
                .DefinePrimitive("list?", (args, caller) => SchemeBoolean.Truth(IsList(First(args))), 1)
                //// <r4rs section="6.3">(member <obj> <list>)</r4rs>
                .DefinePrimitive("member", (args, caller) => MemberAssoc(First(args), Second(args), 'm', ' '), 2)
                //// <r4rs section="6.3">(memq <obj> <list>)</r4rs>
                .DefinePrimitive("memq", (args, caller) => MemberAssoc(First(args), Second(args), 'm', 'q'), 2)
                //// <r4rs section="6.3">(memv <obj> <list>)</r4rs>
                .DefinePrimitive("memv", (args, caller) => MemberAssoc(First(args), Second(args), 'm', 'v'), 2)
                //// <r4rs section="6.3">(pair? <obj>)</r4rs>
                .DefinePrimitive("pair?", (args, caller) => SchemeBoolean.Truth(Pair.IsType(First(args))), 1)
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
            if (Pair.IsType(x))
            {
                ((Pair)x).FirstCell = y;
                return Undefined.Instance;
            }

            return ErrorHandlers.SemanticError("Attempt to set-car! of a non-Pair: " + SchemeString.AsString(x));
        }

        /// <summary>
        /// Set the second member of a pair (the rest) destructively.
        /// </summary>
        /// <param name="x">The pair whose second member we want to modify.</param>
        /// <param name="y">The new value to put into it.</param>
        /// <returns>The obj that has just been modified.</returns>
        private static Obj SetRest(Obj x, Obj y)
        {
            if (Pair.IsType(x))
            {
                ((Pair)x).RestCell = y;
                return Undefined.Instance;
            }

            return ErrorHandlers.SemanticError("Attempt to set-cdr! of a non-Pair: " + SchemeString.AsString(x));
        }

        /// <summary>
        /// Create a list containing objs in the given list in the reverse order.
        /// </summary>
        /// <param name="x">The list to reverse.</param>
        /// <returns>The reversed list.</returns>
        private static Obj Reverse(Obj x)
        {
            Obj result = EmptyList.Instance;
            while (Pair.IsType(x))
            {
                result = Cons(First(x), result);
                x = ((Pair)x).RestCell;
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
        private static Obj Append(Obj args)
        {
            if (EmptyList.IsType(args))
            {
                return EmptyList.Instance;
            }

            Pair result = MakeEmptyList();
            Pair accum = result;

            while (!EmptyList.IsType(Rest(args)))
            {
                accum = Append(accum, First(args));
                args = Rest(args);
            }

            accum.RestCell = First(args);

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
            while (!EmptyList.IsType(toCopy))
            {
                tail.RestCell = MakeList(First(toCopy));
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
                if (EmptyList.IsType(x))
                {
                    return true;
                }

                if (!Pair.IsType(x))
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
            while (Pair.IsType(list))
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

        #region Private Class

        #endregion
    }
}
