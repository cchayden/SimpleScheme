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
    public abstract class Procedure : ListPrimitives
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
                //// <r4rs section="6.9">(apply <proc> <args>)</r4rs>
                //// <r4rs section="6.9">(apply <proc> <arg1> ... <args>)</r4rs>
                .DefinePrimitive("apply", (caller, args) => Proc(First(args)).Apply(ListStar(Rest(args)), caller), 2, MaxInt)
                //// <r4rs section="6.9"> (call-with-current-continuation <proc>)</r4rs>
                .DefinePrimitive(
                    "call-with-current-continuation",
                    (caller, args) => Proc(First(args)).Apply(MakeList(new Continuation(EvaluateContinuation.Call(First(args), caller))), caller),
                    1)
                .DefinePrimitive(
                    "call/cc",
                    (caller, args) => Proc(First(args)).Apply(MakeList(new Continuation(EvaluateContinuation.Call(First(args), caller))), caller),
                    1)

                 // Instead of returning a value, return an evaulator that can be run to get the value
                .DefinePrimitive("eval", (caller, args) => EvaluateExpression.Call(First(args), caller.Env, caller), 1, 2)
                //// <r4rs section="6.9">(force <promise>)</r4rs>
                .DefinePrimitive(
                   "force",
                   (caller, args) =>
                   {
                       object first = First(args);
                       return !(first is Procedure) ? first : Proc(first).Apply(null, caller);
                   },
                   1)
                //// <r4rs section="6.9">(for-each <proc> <list1> <list2> ...)</r4rs>
                .DefinePrimitive("for-each", (caller, args) => EvaluateMap.Call(Proc(First(args)), Rest(args), false, caller), 1, MaxInt)
                //// <r4rs section="6.9">(map proc <list1> <list2> ...)</r4rs>
                .DefinePrimitive("map", (caller, args) => EvaluateMap.Call(Proc(First(args)), Rest(args), true, caller), 1, MaxInt)
                //// <r4rs section="6.9">(procedure? <obj>)</r4rs>
                .DefinePrimitive("procedure?", (caller, args) => SchemeBoolean.Truth(First(args) is Procedure), 1);
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
        /// All subclasses have to be able to apply the procedure to arguments.
        /// </summary>
        /// <param name="args">The arguments to the procedure, which have 
        ///   been evaluated.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next step to run after the application.</returns>
        public abstract Stepper Apply(object args, Stepper caller);

        /// <summary>
        /// Evaluate the procedure.
        /// Macro, Closure, and other procs are evaluated differently.
        /// </summary>
        /// <param name="args">The expression to evaluate.</param>
        /// <param name="caller">Return here when done.</param>
        /// <returns>The next step toexecute.</returns>
        public Stepper Evaluate(object args, Stepper caller)
        {
            // If the function is a macro, expand it and then continue.
            if (this is Macro)
            {
                return EvaluateExpandMacro.Call((Macro)this, args, caller);
            }

            // If the function is a closure, then create a new environment consisting of
            //   1 the closure param list
            //   2 arguments evaluated in the original environment
            //   3 the closure's environment
            // Then continue evaluating the closure body in this new environment
            if (this is Closure)
            {
                // CLOSURE CALL -- capture the environment and evaluate the body
                return EvaluateClosure.Call((Closure)this, args, caller);
            }

            // This is a procedure call.
            // In any other case, the function is a primitive, a continuation, or a ClrProcedure.
            // Evaluate the arguments in the environment, then apply the function 
            //    to the arguments.
            return EvaluateProc.Call(this, args, caller);
        }
    }
}