// <copyright file="Procedure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Text;

    /// <summary>
    /// A procedure is executable. 
    /// It supports an Apply method.
    /// Lambdas, Continuations, CLR methods, and primitives are examples of Procedures.
    /// </summary>
    public abstract class Procedure : SchemeObject
    {
        #region Constants
        /// <summary>
        /// The default name of a procedure.
        /// </summary>
        private const string AnonymousProc = "anonymous procedure";
        #endregion

        #region Fields

        /// <summary>
        /// The minimum number of args for the procedure.
        /// Set only in the constructor here and in subclasses.
        /// </summary>
        private int minArgs;

        /// <summary>
        /// The maximum number of args for the procedure.
        /// Set only in the constructor here and in subclasses.
        /// </summary>
        private int maxArgs;
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
            this.minArgs = minArgs;
            this.maxArgs = maxArgs;
        }
        #endregion

        #region Accessors

        /// <summary>
        /// Gets all Procedures have a name.  It can be set only by the subclass.
        /// </summary>
        public string ProcedureName { get; private set; }

        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the procedure primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static new void DefinePrimitives(PrimitiveEnvironment env)
        {
            const int MaxInt = int.MaxValue;
            env
                .DefinePrimitive(
                        "apply", 
                        new[] { "6.9", "(apply <proc> <args>)", "(apply <proc> <arg1> ... <args>)" },
                        (args, caller) => ((Procedure)First(args)).Apply(ListStar(Rest(args)), caller), 
                        2, 
                        MaxInt, 
                        Primitive.ArgType.Proc, 
                        Primitive.ArgType.Obj)
                .DefinePrimitive(
                        "call-with-current-continuation", 
                        new[] { "6.9", "(call-with-current-continuation <proc>)" },
                        (args, caller) => ((Procedure)First(args)).CallCc(caller), 
                        1, 
                        Primitive.ArgType.Proc)
                .DefinePrimitive(
                        "call/cc", 
                        new[] { "(call/cc <proc>)" },
                        (args, caller) => ((Procedure)First(args)).CallCc(caller), 
                        1, 
                        Primitive.ArgType.Proc)

                .DefinePrimitive(
                        "force", 
                        new[] { "6.9", "(force <promise>)" },
                        (args, caller) => Force((Procedure)First(args), caller), 
                        1, 
                        Primitive.ArgType.Proc)
                ////  Note: list(s) are optional and may be empty lists.
                .DefinePrimitive(
                        "for-each", 
                        new[] { "6.9", "(for-each <proc> <list1> <list2> ...)" },
                        (args, caller) => EvaluateMap.Call((Procedure)First(args), Rest(args), false, caller.Env, caller), 
                         1, 
                        MaxInt,
                        Primitive.ArgType.Proc, 
                        Primitive.ArgType.PairOrEmpty)
                ////  Note: list(s) are optional and may be empty lists.
                .DefinePrimitive(
                        "map", 
                        new[] { "6.9", "(map <proc> <list1> <list2> ...)" },
                        (args, caller) => EvaluateMap.Call((Procedure)First(args), Rest(args), true, caller.Env, caller), 
                        1, 
                        MaxInt, 
                        Primitive.ArgType.Proc, 
                        Primitive.ArgType.PairOrEmpty)
                .DefinePrimitive(
                        "procedure?", 
                        new[] { "6.9", "(procedure? <obj>)" },
                        (args, caller) => SchemeBoolean.Truth(First(args) is Procedure), 
                        1, 
                        Primitive.ArgType.Obj);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write the procedure to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public override void PrintString(bool quoted, StringBuilder buf)
        {
            buf.Append(this.ToString());
        }

        /// <summary>
        /// Describe a procedure by returning its value.
        /// </summary>
        /// <returns>The procedure as a string.</returns>
        public override string Describe()
        {
            var sb = new StringBuilder();
            this.PrintString(false, sb);
            return sb.ToString();
        }

        /// <summary>
        /// Display the procedure as a string.
        /// Since there is nothing to show, at least give the type.
        /// </summary>
        /// <returns>The procedure type name.</returns>
        public override string ToString()
        {
            return "<procedure>";
        }

        /// <summary>
        /// All subclasses have to be able to apply the procedure to arguments.
        /// That is what it means to be a procedure.
        /// </summary>
        /// <param name="args">The arguments to the procedure, which may or may not have 
        ///   been evaluated.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next evaluator to run after the application.</returns>
        public abstract Evaluator Apply(SchemeObject args, Evaluator caller);

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
        public virtual Evaluator Evaluate(SchemeObject args, Environment env, Evaluator caller)
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

        #region Protected Methods
        /// <summary>
        /// Set the min and max number of arguments accepted.
        /// </summary>
        /// <param name="minCount">The minimum number of args accepted.</param>
        /// <param name="maxCount">The maximum number of args accepted.</param>
        protected void SetMinMax(int minCount, int maxCount)
        {
            this.minArgs = minCount;
            this.maxArgs = maxCount;
        }

        /// <summary>
        /// Set the number of arguments accepted.
        /// </summary>
        /// <param name="count">The number of args accepted.</param>
        protected void SetMinMax(int count)
        {
            this.minArgs = count;
            this.maxArgs = count;
        }

        /// <summary>
        /// Check the number of args passed.
        /// </summary>
        /// <param name="args">The arguments passed to the procedure.</param>
        /// <param name="tagType">Name, for the error message.</param>
        /// <returns>The number of arguments passed to the procedure.</returns>
        protected int CheckArgs(SchemeObject args, Type tagType)
        {
            int numArgs = ListLength(args);
            if (numArgs < this.minArgs || numArgs > this.maxArgs)
            {
                string tag = tagType.SchemeTypeName();
                string msg = numArgs < this.minArgs ? "few" : "many";
                ErrorHandlers.SemanticError(
                    string.Format(
                       @"""{0}"" too {1} args ({2}) for {3}: ""{4}""", 
                      tag, 
                      msg, 
                      numArgs, 
                      this.ProcedureName, 
                      args));
            }

            return numArgs;
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Force a promise.  The promise is a proc: apply it.
        /// </summary>
        /// <param name="promise">A proc that will produce the result.</param>
        /// <param name="caller">The caller.</param>
        /// <returns>The result of applying the proc.</returns>
        private static EvaluatorOrObject Force(Procedure promise, Evaluator caller)
        {
            return promise.Apply(null, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Perform the call/cc primitive.
        /// Create a continuation that captures the caller's environment and returns to the caller.
        /// Then apply this procedure to it.
        /// Clean the lambda first, because it could have been previously executed in another environment.
        /// </summary>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>A function to continue the evaluation.</returns>
        private EvaluatorOrObject CallCc(Evaluator caller)
        {
            return this.Apply(List.MakeList(Continuation.New(caller)), caller);
        }
        #endregion

        /// <summary>
        /// Ensure that the given object is a procedure.
        /// Used for checking before attempting to Apply a computed value.
        /// </summary>
        /// <param name="obj">The object to check</param>
        public static void EnsureProcedure(SchemeObject obj)
        {
            if (!(obj is Procedure))
            {
                ErrorHandlers.ProcError("Attempt to Apply non-proc", obj);
            }

        }
    }
}