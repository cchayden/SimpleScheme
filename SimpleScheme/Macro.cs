// <copyright file="Macro.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Represents a macro definition.
    /// </summary>
    public sealed class Macro : Closure
    {
        /// <summary>
        /// Initializes a new instance of the Macro class.
        /// </summary>
        /// <param name="parms">The macro params.</param>
        /// <param name="body">The macro body.</param>
        /// <param name="env">The environment that the macro is defined in.</param>
        public Macro(object parms, object body, Environment env)
            : base(parms, body, env)
        {
        }

        /// <summary>
        /// Exapnd the macro.
        /// </summary>
        /// <param name="interpreter">The interpreter instance the expansion 
        ///    takes place in.</param>
        /// <param name="x">The macro args, consisting of the macro name and args.</param>
        /// <returns>The result of macro expansion.</returns>
        public static object MacroExpand(Scheme interpreter, Evaluator parent, object x)
        {
            if (!(x is Pair))
            {
                return x;
            }

            object fn = interpreter.Eval(First(x), interpreter.GlobalEnvironment);
            return !(fn is Macro) ? x : ((Macro)fn).Expand(interpreter, parent, (Pair)x, Rest(x));
        }

        /// <summary>
        /// Expand the macro.
        /// </summary>
        /// <param name="interpreter">The interpreter instance the expansion takes 
        ///    place in.</param>
        /// <param name="oldPair">???</param>
        /// <param name="args">The macro args.</param>
        /// <returns>The result of expanding the macro.</returns>
        public Pair Expand(Scheme interpreter, Evaluator parent, Pair oldPair, object args)
        {
            object expansion = Apply(interpreter, parent, args);
            if (expansion is Pair)
            {
                oldPair.First = ((Pair)expansion).First;
                oldPair.Rest = ((Pair)expansion).Rest;
            }
            else
            {
                oldPair.First = "begin";
                oldPair.Rest = Cons(expansion, null);
            }

            return oldPair;
        }
    }
}