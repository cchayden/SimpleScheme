// <copyright file="List.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// List utilities used by the primitives.
    /// </summary>
    public sealed class List : ListPrimitives
    {
        /// <summary>
        /// Do all the combination car-cdr functions.
        /// </summary>
        /// <param name="name">The function name.</param>
        /// <param name="args">The expression to operate on.</param>
        /// <returns>The result of the operation.</returns>
        public static object Cxr(string name, object args)
        {
            object first = First(args);
            for (int i = name.Length - 2; i >= 1; i--)
            {
                first = name[i] == 'a' ? First(first) : Rest(first);
            }

            return first;
        }

        /// <summary>
        /// Define the list primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(Environment env)
        {
            const int MaxInt = int.MaxValue;
            env
                //// <r4rs section="6.3">(append <list> ...)</r4rs>
                .DefinePrimitive("append", (caller, args) => args == null ? null : Append(args), 0, MaxInt)
                //// <r4rs section="6.3">(assoc <obj> <alist>)</r4rs>
                .DefinePrimitive("assoc", (caller, args) => MemberAssoc(First(args), Second(args), 'a', ' '), 2)
                //// <r4rs section="6.3">(assq <obj> <alist>)</r4rs>
                .DefinePrimitive("assq", (caller, args) => MemberAssoc(First(args), Second(args), 'a', 'q'), 2)
                //// <r4rs section="6.3">(assv <obj> <alist>)</r4rs>
                .DefinePrimitive("assv", (caller, args) => MemberAssoc(First(args), Second(args), 'a', 'v'), 2)
                //// <r4rs section="6.3">(car <pair>)</r4rs>
                .DefinePrimitive("car", (caller, args) => First(First(args)), 1)
                .DefinePrimitive("first", (caller, args) => First(First(args)), 1)
                .DefinePrimitive("second", (caller, args) => Second(First(args)), 1)
                .DefinePrimitive("third", (caller, args) => Third(First(args)), 1)
                //// <r4rs section="6.3">(cdr <pair>)</r4rs>
                .DefinePrimitive("cdr", (caller, args) => Rest(First(args)), 1)
                .DefinePrimitive("rest", (caller, args) => Rest(First(args)), 1)
                //// <r4rs section="6.3">(cons <obj1> <obj2>)</r4rs>
                .DefinePrimitive("cons", (caller, args) => Cons(First(args), Second(args)), 2)
                //// <r4rs section="6.3">(length <list> ...)</r4rs>
                .DefinePrimitive("length", (caller, args) => Number.Num(Length(First(args))), 1)
                //// <r4rs section="6.3">(list <obj> ...)</r4rs>
                .DefinePrimitive("list", (caller, args) => args, 0, MaxInt)
                //// <r4rs section="6.7">(list->string <chars>)</r4rs>
                .DefinePrimitive("list->string", (caller, args) => SchemeString.ListToString(First(args)), 1)
                //// <r4rs section="6.8">(list->vector <vector>)</r4rs>
                .DefinePrimitive("list->vector", (caller, args) => Vector.MakeVector(First(args)), 1)
                //// <r4rs section="6.3">(list-ref <list> <k>)</r4rs>
                .DefinePrimitive(
                   "list-ref",
                   (caller, args) =>
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
                //// <r4rs section="6.3">(list-tail <list> <k>)</r4rs>
                .DefinePrimitive(
                   "list-tail",
                   (caller, args) =>
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
                //// <r4rs section="6.3">(list? <obj>)</r4rs>
                .DefinePrimitive("list?", (caller, args) => SchemeBoolean.Truth(IsList(First(args))), 1)
                //// <r4rs section="6.3">(member <obj> <list>)</r4rs>
                .DefinePrimitive("member", (caller, args) => MemberAssoc(First(args), Second(args), 'm', ' '), 2)
                //// <r4rs section="6.3">(memq <obj> <list>)</r4rs>
                .DefinePrimitive("memq", (caller, args) => MemberAssoc(First(args), Second(args), 'm', 'q'), 2)
                //// <r4rs section="6.3">(memv <obj> <list>)</r4rs>
                .DefinePrimitive("memv", (caller, args) => MemberAssoc(First(args), Second(args), 'm', 'v'), 2)
                //// <r4rs section="6.3">(pair? <obj>)</r4rs>
                .DefinePrimitive("pair?", (caller, args) => SchemeBoolean.Truth(First(args) is Pair), 1)
                //// <r4rs section="6.3">(reverse <list>)</r4rs>
                .DefinePrimitive("reverse", (caller, args) => Reverse(First(args)), 1)
                //// <r4rs section="6.3">(set-car! <pair> <obj>)</r4rs>
                .DefinePrimitive("set-car!", (caller, args) => SetFirst(First(args), Second(args)), 2)
                .DefinePrimitive("set-first!", (caller, args) => SetFirst(First(args), Second(args)), 2)
                //// <r4rs section="6.3">(set-cdr! <pair> <obj>)</r4rs>
                .DefinePrimitive("set-cdr!", (caller, args) => SetRest(First(args), Second(args)), 2)
                .DefinePrimitive("set-rest!", (caller, args) => SetRest(First(args), Second(args)), 2);

            DefineAccessPrimitives(env, "aa");
            DefineAccessPrimitives(env, "ad");
            DefineAccessPrimitives(env, "da");
            DefineAccessPrimitives(env, "dd");
        }

        /// <summary>
        /// Define all the combinations of c[ad]+r functions up to four levels.
        /// </summary>
        /// <param name="env">The environment to define the functions in.</param>
        /// <param name="access">The access string so far.</param>
        private static void DefineAccessPrimitives(Environment env, string access)
        {
            string prim = "c" + access + "r";
            env.DefinePrimitive(prim, (caller, args) => Cxr(prim, args), 1);
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
        /// <returns>The object that has just been modified.</returns>
        private static object SetFirst(object x, object y)
        {
            return x is Pair ? 
                ((Pair)x).FirstCell = y : 
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
            return x is Pair ? 
                ((Pair)x).RestCell = y : 
                ErrorHandlers.Error("SetRest: attempt to set-cdr of a non-Pair: " + SchemeString.AsString(x));
        }

        /// <summary>
        /// Create a list containing objects in the given list in the reverse order.
        /// Avoid Pair iterator for speed.
        /// </summary>
        /// <param name="x">The list to reverse.</param>
        /// <returns>The reversed list.</returns>
        private static object Reverse(object x)
        {
            object result = null;
            while (x is Pair)
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
        private static object Append(object args)
        {
            Pair result = MakeList(null);
            Pair accum = result;

            while (Rest(args) != null)
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
        private static Pair Append(Pair tail, object toCopy)
        {
            while (toCopy != null)
            {
                tail.RestCell = MakeList(First(toCopy));
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
