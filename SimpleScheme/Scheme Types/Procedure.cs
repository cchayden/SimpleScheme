// <copyright file="Procedure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// A procedure is executable. 
    /// It supports an Apply method.
    /// Lambdas, Continuations, CLR methods, and primitives are examples of Procedures.
    /// </summary>
    public abstract class Procedure : Printable
    {
        #region Constants
        /// <summary>
        /// The printable name of the procedure type.
        /// </summary>
        public const string Name = "procedure";

        /// <summary>
        /// The default name of a procedure.
        /// </summary>
        private const string AnonymousProc = "anonymous procedure";
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Procedure class, setting min and max args.
        /// </summary>
        /// <param name="minArgs">The minimum number of args.</param>
        /// <param name="maxArgs">The maximum number of args.</param>
        protected Procedure(int minArgs, int maxArgs)
        {
            this.ProcedureName = AnonymousProc;
            this.MinArgs = minArgs;
            this.MaxArgs = maxArgs;
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets or sets all Procedures have a name.  It can be set only by the subclass.
        /// Can't figure out how to make this public rather than public.
        /// </summary>
        public string ProcedureName { get; protected set; }

        /// <summary>
        /// Gets or sets the minimum number of arguments permitted.
        /// Set only during construction.
        /// </summary>
        protected int MinArgs { get; set; }

        /// <summary>
        /// Gets or sets the maximum number of arguments permitted.
        /// Set only during construction.
        /// </summary>
        protected int MaxArgs { get; set; }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Tests whether to given object is a procedure.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme procedure.</returns>
        public static bool Is(Obj obj)
        {
            return obj is Procedure;
        }

        /// <summary>
        /// Cast the given object to a procedure.
        /// </summary>
        /// <param name="x">The obj to test.</param>
        /// <returns>The procedure.</returns>
        public static Procedure As(Obj x)
        {
            if (Is(x))
            {
                return (Procedure)x;
            }

            ErrorHandlers.TypeError(Name, x);
            return null;
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the procedure primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(PrimitiveEnvironment env)
        {
            const int MaxInt = Int32.MaxValue;
            env
                //// <r4rs section="6.9">(apply <proc> <args>)</r4rs>
                //// <r4rs section="6.9">(apply <proc> <arg1> ... <args>)</r4rs>
                .DefinePrimitive("apply", (args, caller) => As(List.First(args)).Apply(List.ListStar(List.Rest(args)), caller), 2, MaxInt)
                //// <r4rs section="6.9"> (call-with-current-continuation <proc>)</r4rs>
                .DefinePrimitive("call-with-current-continuation", (args, caller) => As(List.First(args)).CallCc(caller), 1)
                .DefinePrimitive("call/cc", (args, caller) => As(List.First(args)).CallCc(caller), 1)

                //// <r4rs section="6.9">(force <promise>)</r4rs>
                .DefinePrimitive("force", (args, caller) => Force(List.First(args), caller), 1)
                //// <r4rs section="6.9">(for-each <proc> <list1> <list2> ...)</r4rs>
                .DefinePrimitive("for-each", (args, caller) => EvaluateMap.Call(As(List.First(args)), List.Rest(args), false, caller.Env, caller), 1, MaxInt)
                //// <r4rs section="6.9">(map proc <list1> <list2> ...)</r4rs>
                .DefinePrimitive("map", (args, caller) => EvaluateMap.Call(As(List.First(args)), List.Rest(args), true, caller.Env, caller), 1, MaxInt)
                //// <r4rs section="6.9">(procedure? <obj>)</r4rs>
                .DefinePrimitive("procedure?", (args, caller) => SchemeBoolean.Truth(Is(List.First(args))), 1);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write the procedure to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public override void AsString(bool quoted, StringBuilder buf)
        {
            buf.Append(this.ToString());
        }

        /// <summary>
        /// Display the procedure as a string.
        /// Since there is nothing to show, at least give the type.
        /// </summary>
        /// <returns>The procedure type name.</returns>
        public override string ToString()
        {
            return "<" + Name + ">";
        }

        /// <summary>
        /// All subclasses have to be able to apply the procedure to arguments.
        /// That is what it means to be a procedure.
        /// </summary>
        /// <param name="args">The arguments to the procedure, which may or may not have 
        ///   been evaluated.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next evaluator to run after the application.</returns>
        public abstract Evaluator Apply(object args, Evaluator caller);

        /// <summary>
        /// Evaluate the procedure.
        /// At this point, the args are NOT evaluated
        /// Macro is evaluated differently, and overrides this method.
        /// This is a primitive, a lambda, a continuation, or a CLR Procedure.
        /// Evaluate the arguments in the environment, then apply the function 
        ///    to the arguments.
        /// </summary>
        /// <param name="args">The arguments to the procedure.</param>
        /// <param name="env">The environment to use for the application.</param>
        /// <param name="caller">Return here when done.</param>
        /// <returns>The next evaluator to execute.</returns>
        public virtual Evaluator Evaluate(Obj args, Environment env, Evaluator caller)
        {
            return EvaluateProc.Call(this, args, env, caller);
        }

        /// <summary>
        /// Assign the procedure name.  If the name is still the default, assign it 
        ///    the name given in the argument.
        /// This is used to associate names with lambdas.  If a "define" is defining
        ///   a lambda, then the name field of the lambda is set as a side effect.
        /// Not all lambdas are defined however, so it may remain anonymous.
        /// </summary>
        /// <param name="name">The name to assign it.</param>
        public void SetName(string name)
        {
            if (this.ProcedureName == AnonymousProc)
            {
                this.ProcedureName = name;
            }
        }
        #endregion

        /// <summary>
        /// Check the number of args passed.
        /// </summary>
        /// <param name="args">The arguments passed to the procedure.</param>
        /// <param name="tag">Name, for the error message.</param>
        protected void CheckArgs(Obj args, string tag)
        {
            int numArgs = List.Length(args);
            if (numArgs < this.MinArgs)
            {
                ErrorHandlers.SemanticError(tag + ": too few args, " + numArgs + ", for " +
                                                            this.ProcedureName + ": " + args);
            }

            if (numArgs > this.MaxArgs)
            {
                ErrorHandlers.SemanticError(tag + ": too many args, " + numArgs + ", for " +
                                                            this.ProcedureName + ": " + args);
            }
        }

        #region Private Static Methods
        /// <summary>
        /// Force a promise.  The promise is a proc: apply it.
        /// </summary>
        /// <param name="promise">A proc that will produce the result.</param>
        /// <param name="caller">The caller.</param>
        /// <returns>The result of applying the proc.</returns>
        private static Obj Force(Obj promise, Evaluator caller)
        {
            return !Is(promise) ? promise : As(promise).Apply(null, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Perform the call/cc primitive.
        /// Create a continuation that captures the caller's environment and returns to the caller.
        /// Then apply this procedure to it.
        /// </summary>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>A function to continue the evaluation.</returns>
        private Obj CallCc(Evaluator caller)
        {
            return this.Apply(List.New(new Continuation(caller)), caller);
        }
        #endregion
    }
}