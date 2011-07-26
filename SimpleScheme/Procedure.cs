// <copyright file="Procedure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// A procedure is executable. 
    /// It supports an Apply method.
    /// Closures, Continuations, CLR methods, and primitives are examples of Procedures.
    /// </summary>
    public abstract class Procedure
    {
        #region Constants
        /// <summary>
        /// The default name of a procedure.
        /// </summary>
        private const string AnonymousProc = "anonymous procedure";
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Procedure class.
        /// Sets the name to the default.
        /// </summary>
        protected Procedure()
        {
            this.Name = AnonymousProc;
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets or sets all Procedures have a name.  It can be set only by the subclass.
        /// Can't figure out how to make this internal rather than public.
        /// </summary>
        public string Name { get; protected set; }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the procedure primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        internal static void DefinePrimitives(PrimitiveEnvironment env)
        {
            const int MaxInt = int.MaxValue;
            env
                //// <r4rs section="6.9">(apply <proc> <args>)</r4rs>
                //// <r4rs section="6.9">(apply <proc> <arg1> ... <args>)</r4rs>
                .DefinePrimitive("apply", (args, caller) => ApplyPrimitive(List.First(args), List.Rest(args), caller), 2, MaxInt)
                //// <r4rs section="6.9"> (call-with-current-continuation <proc>)</r4rs>
                .DefinePrimitive("call-with-current-continuation", (args, caller) => CallCc(List.First(args), caller), 1)
                .DefinePrimitive("call/cc", (args, caller) => CallCc(List.First(args), caller), 1)

                //// <r4rs section="6.9">(force <promise>)</r4rs>
                .DefinePrimitive("force", (args, caller) => Force(List.First(args), caller), 1)
                //// <r4rs section="6.9">(for-each <proc> <list1> <list2> ...)</r4rs>
                .DefinePrimitive("for-each", (args, caller) => EvaluateMap.Call(Proc(List.First(args)), List.Rest(args), false, caller.Env, caller), 1, MaxInt)
                //// <r4rs section="6.9">(map proc <list1> <list2> ...)</r4rs>
                .DefinePrimitive("map", (args, caller) => EvaluateMap.Call(Proc(List.First(args)), List.Rest(args), true, caller.Env, caller), 1, MaxInt)
                //// <r4rs section="6.9">(procedure? <obj>)</r4rs>
                .DefinePrimitive("procedure?", (args, caller) => SchemeBoolean.Truth(TypePrimitives.IsProcedure(List.First(args))), 1);
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Check that the given object is a procedure.
        /// </summary>
        /// <param name="x">The obj to test.</param>
        /// <returns>The procedure.</returns>
        internal static Procedure Proc(Obj x)
        {
            if (TypePrimitives.IsProcedure(x))
            {
                return (Procedure)x;
            }

            return Proc(ErrorHandlers.TypeError(TypePrimitives.ProcedureName, x));
        }

        /// <summary>
        /// Force a promise.  The promise is a proc: apply it.
        /// </summary>
        /// <param name="promise">A proc that will produce the result.</param>
        /// <param name="caller">The caller.</param>
        /// <returns>The result of applying the proc.</returns>
        internal static Obj Force(Obj promise, Stepper caller)
        {
            return !TypePrimitives.IsProcedure(promise) ? promise : Proc(promise).Apply(null, caller);
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// All subclasses have to be able to apply the procedure to arguments.
        /// </summary>
        /// <param name="args">The arguments to the procedure, which have 
        ///   been evaluated.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next step to run after the application.</returns>
        internal abstract Stepper Apply(object args, Stepper caller);

        /// <summary>
        /// Assign the procedure name.  If the name is still the default, assign it 
        ///    the name given in the argument.
        /// </summary>
        /// <param name="name">The name to assign it.</param>
        internal void SetName(string name)
        {
            if (this.Name == AnonymousProc)
            {
                this.Name = name;
            }
        }

        /// <summary>
        /// Evaluate the procedure.
        /// Macro, Closure, and other procs are evaluated differently.
        /// </summary>
        /// <param name="args">The expression to evaluate.</param>
        /// <param name="env">The environment to use for the application.</param>
        /// <param name="caller">Return here when done.</param>
        /// <returns>The next step toexecute.</returns>
        internal Stepper Evaluate(Obj args, Environment env, Stepper caller)
        {
            // If the function is a macro, expand it and then continue.
            if (TypePrimitives.IsMacro(this))
            {
                return EvaluateExpandMacro.Call((Macro)this, args, env, caller);
            }

            // If the function is a closure, then create a new environment consisting of
            //   1 the closure param list
            //   2 arguments evaluated in the original environment
            //   3 the closure's environment
            // Then continue evaluating the closure body in this new environment
            if (TypePrimitives.IsClosure(this))
            {
                // CLOSURE CALL -- capture the environment and evaluate the body
                return EvaluateClosure.Call((Closure)this, args, env, caller);
            }

            // This is a procedure call.
            // In any other case, the function is a primitive, a continuation, or a ClrProcedure.
            // Evaluate the arguments in the environment, then apply the function 
            //    to the arguments.
            return EvaluateProc.Call(this, args, env, caller);
        }
        #endregion

        #region Private Methods
        private static Stepper ApplyPrimitive(Obj proc, Obj args, Stepper caller)
        {
            return Proc(proc).Apply(List.ListStar(args), caller);
        }

        /// <summary>
        /// Perform the call/cc primitive.
        /// Create a continuation that captures the caller's environment and returns to the caller.
        /// The continuation itself is a sequence that evaluates the proc in the caller's environment and returns to the caller.
        /// </summary>
        /// <param name="proc">The expression to evaluate.</param>
        /// <param name="caller">The calling stepper.</param>
        /// <returns>A function to continue the evaluation.</returns>
        private static Obj CallCc(Obj proc, Stepper caller)
        {
            return Proc(proc).Apply(
                List.New(new Continuation(EvaluateContinuation.Call(proc, caller.Env, caller))), caller);
        }
        #endregion
    }
}