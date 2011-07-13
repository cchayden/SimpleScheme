﻿// <copyright file="Procedure.cs" company="Charles Hayden">
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
    internal abstract class Procedure : ListPrimitives
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
        internal static void DefinePrimitives(Environment env)
        {
            const int MaxInt = int.MaxValue;
            env
                //// <r4rs section="6.9">(apply <proc> <args>)</r4rs>
                //// <r4rs section="6.9">(apply <proc> <arg1> ... <args>)</r4rs>
                .DefinePrimitive("apply", (args, caller) => Proc(First(args)).Apply(ListStar(Rest(args)), caller.Env, caller), 2, MaxInt)
                //// <r4rs section="6.9"> (call-with-current-continuation <proc>)</r4rs>
                .DefinePrimitive(
                    "call-with-current-continuation",
                    (args, caller) => Proc(First(args)).Apply(MakeList(Continuation.New(EvaluateContinuation.Call(First(args), caller.Env, caller))), caller.Env, caller),
                    1)
                .DefinePrimitive(
                    "call/cc",
                    (args, caller) => Proc(First(args)).Apply(MakeList(Continuation.New(EvaluateContinuation.Call(First(args), caller.Env, caller))), caller.Env, caller),
                    1)

                //// <r4rs section="6.9">(force <promise>)</r4rs>
                .DefinePrimitive("force", (args, caller) => Force(First(args), caller), 1)
                //// <r4rs section="6.9">(for-each <proc> <list1> <list2> ...)</r4rs>
                .DefinePrimitive("for-each", (args, caller) => EvaluateMap.Call(Proc(First(args)), Rest(args), false, caller.Env, caller), 1, MaxInt)
                //// <r4rs section="6.9">(map proc <list1> <list2> ...)</r4rs>
                .DefinePrimitive("map", (args, caller) => EvaluateMap.Call(Proc(First(args)), Rest(args), true, caller.Env, caller), 1, MaxInt)
                //// <r4rs section="6.9">(procedure? <obj>)</r4rs>
                .DefinePrimitive("procedure?", (args, caller) => SchemeBoolean.Truth(IsType(First(args))), 1);
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Test an object's type.
        /// </summary>
        /// <param name="obj">The object to test.</param>
        /// <returns>True if the object is a scheme procedure.</returns>
        internal static bool IsType(Obj obj)
        {
            return obj is Procedure;
        }

        /// <summary>
        /// Give the name of the type (for display).
        /// </summary>
        /// <returns>The type name.</returns>
        internal static string TypeName()
        {
            return "procedure";
        }

        /// <summary>
        /// Check that the given object is a procedure.
        /// </summary>
        /// <param name="x">The obj to test.</param>
        /// <returns>The procedure.</returns>
        internal static Procedure Proc(Obj x)
        {
            if (IsType(x))
            {
                return (Procedure)x;
            }

            return Proc(ErrorHandlers.TypeError("procedure", x));
        }

        /// <summary>
        /// Force a promise.  The promise is a proc: apply it.
        /// </summary>
        /// <param name="promise">A proc that will produce the result.</param>
        /// <param name="caller">The caller.</param>
        /// <returns>The result of applying the proc.</returns>
        internal static Obj Force(Obj promise, Stepper caller)
        {
            return !IsType(promise) ? promise : Proc(promise).Apply(null, caller.Env, caller);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// All subclasses have to be able to apply the procedure to arguments.
        /// </summary>
        /// <param name="args">The arguments to the procedure, which have 
        ///   been evaluated.</param>
        /// <param name="env">The environment to use for the application.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next step to run after the application.</returns>
        internal abstract Stepper Apply(Obj args, Environment env, Stepper caller);

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
            if (Macro.IsType(this))
            {
                return EvaluateExpandMacro.Call((Macro)this, args, env, caller);
            }

            // If the function is a closure, then create a new environment consisting of
            //   1 the closure param list
            //   2 arguments evaluated in the original environment
            //   3 the closure's environment
            // Then continue evaluating the closure body in this new environment
            if (Closure.IsType(this))
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
    }
}