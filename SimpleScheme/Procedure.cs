// <copyright file="Procedure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// A procedure is executable. 
    /// It supports an Apply method.
    /// Closures, Continuations, CLR methods, and primitives are examples of Procedures.
    /// </summary>
    public abstract class Procedure
    {
        /// <summary>
        /// The default name of a procedure.
        /// </summary>
        private const string AnonymousProc = "anonymous procedure";

        /// <summary>
        /// Initializes a new instance of the Procedure class.
        /// Sets the name to the default.
        /// </summary>
        protected Procedure()
        {
            this.Name = AnonymousProc;
        }

        /// <summary>
        /// Gets or sets all Procedures have a name.  It can be set only by the subclass.
        /// </summary>
        public string Name { get; protected set; }

        /// <summary>
        /// Define the procedure primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(Environment env)
        {
            const int MaxInt = int.MaxValue;
            env
                .DefinePrimitive("apply", (parent, args) => Proc(List.First(args)).Apply(parent, List.ListStar(List.Rest(args))), 2, MaxInt)
                .DefinePrimitive(
                    "call-with-current-continuation",
                    (parent, args) => Proc(List.First(args)).Apply(
                        parent,
                        List.MakeList(new Continuation(EvaluateContinuation.Call(parent, List.First(args))))),
                    1)
                .DefinePrimitive(
                    "call/cc",
                    (parent, args) => Proc(List.First(args)).Apply(
                        parent,
                        List.MakeList(new Continuation(EvaluateContinuation.Call(parent, List.First(args))))),
                    1)

                 // Instead of returning a value, return an evaulator that can be run to get the value
                .DefinePrimitive("eval", (parent, args) => EvaluatorMain.Call(parent, List.First(args), parent.Env), 1, 2)
                .DefinePrimitive(
                   "force",
                   (parent, args) =>
                   {
                       object first = List.First(args);
                       return !(first is Procedure) ? first : Proc(first).Apply(parent, null);
                   },
                   1)
                .DefinePrimitive("for-each", (parent, args) => EvaluateMap.Call(parent, List.Rest(args), Proc(List.First(args)), null), 1, MaxInt)
                .DefinePrimitive("map", (parent, args) => EvaluateMap.Call(parent, List.Rest(args), Proc(List.First(args)), List.MakeList(null)), 1, MaxInt)
                .DefinePrimitive("procedure?", (parent, args) => SchemeBoolean.Truth(List.First(args) is Procedure), 1);
        }

        /// <summary>
        /// Convert the given object to a procedure.
        /// It should be one already: if not, throw an error.
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The object as a procedure.</returns>
        public static Procedure Proc(object x)
        {
            if (x is Procedure)
            {
                return (Procedure)x;
            }

            return Proc(ErrorHandlers.Error("Not a procedure: " + SchemeString.AsString(x)));
        }

        /// <summary>
        /// Assign the procedure name.  If the name is still the default, assign it 
        ///    the name given in the argument.
        /// </summary>
        /// <param name="name">The name to assign it.</param>
        public void SetName(string name)
        {
            if (this.Name == AnonymousProc)
            {
                this.Name = name;
            }
        }

        /// <summary>
        /// The string form of a proc is its name in curly brackets.
        /// </summary>
        /// <returns>The name of the proc.</returns>
        public override string ToString()
        {
            return "{" + this.Name + "}";
        }

        /// <summary>
        /// All subclasses have to be able to apply the procedure to arguments.
        /// </summary>
        /// <param name="parent">The calling evaluator.</param>
        /// <param name="args">The arguments to the procedure, which have 
        /// been evaluated.</param>
        /// <returns>The result of applying the procedure to the arguments.</returns>
        public abstract object Apply(Stepper parent, object args);
    }
}