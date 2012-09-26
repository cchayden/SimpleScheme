// <copyright file="Procedure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

    /// <summary>
    /// A procedure is executable. 
    /// It supports an Apply method.
    /// Lambdas, Continuations, CLR methods, and primitives are examples of Procedures.
    /// </summary>
    [ContractClass(typeof(ProcedureContract))]
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
        /// If true, the arguments should not be evaluated.
        /// </summary>
        private readonly bool unevaluated;

        /// <summary>
        /// The minimum number of args for the procedure.
        /// Set only in the constructor here and in subclasses.
        /// </summary>
        private readonly int minArgs;

        /// <summary>
        /// The maximum number of args for the procedure.
        /// Set only in the constructor here and in subclasses.
        /// </summary>
        private readonly int maxArgs;

        /// <summary>
        /// The procedure name.  
        /// </summary>
        private string procedureName;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Procedure class, setting min and max args.
        /// </summary>
        /// <param name="name">The procedure name.</param>
        /// <param name="argsInfo">Information about primitive args.</param>
        protected Procedure(string name, ArgsInfo argsInfo)
        {
            this.procedureName = name ?? AnonymousProc;
            this.minArgs = argsInfo.MinArgs;
            this.maxArgs = argsInfo.MaxArgs;
            this.unevaluated = argsInfo.Unevaluated;
        }
        #endregion

        #region Accessors

        /// <summary>
        /// Gets or sets the procedure name.  All Procedures have a name.  It can be set only by the subclass.
        /// This is used to associate names with lambdas.  If a "define" is defining
        ///   a lambda, then the name field of the lambda is set as a side effect.
        /// Not all lambdas are defined however, so it may remain anonymous.
        /// </summary>
        internal string ProcedureName
        {
            get
            {
                Contract.Ensures(Contract.Result<string>() != null);
                return this.procedureName;
            }

            set
            {
                Contract.Requires(value != null);
                if (this.ProcedureName == AnonymousProc)
                {
                    this.procedureName = value;
                }
            }
        }

        /// <summary>
        /// Gets a value indicating whether to evaluate arguments.
        /// </summary>
        internal bool EvaluateArgs
        {
            get { return !this.unevaluated; }
        }

        /// <summary>
        /// Gets the minimum number of arguments.
        /// </summary>
        internal int MinArgs
        {
            get { return this.minArgs; }
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Display the procedure as a string.
        /// Since there is nothing to show, at least give the name.
        /// </summary>
        /// <returns>The procedure type name.</returns>
        public override string ToString()
        {
            return "<procedure " + this.ProcedureName + ">";
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the procedure primitives.
        /// </summary>
        /// <param name="primEnv">The environment to define the primitives into.</param>
        internal static new void DefinePrimitives(PrimitiveEnvironment primEnv)
        {
            Contract.Requires(primEnv != null);
            const int MaxInt = int.MaxValue;
            primEnv
                .DefinePrimitive(
                        "apply", 
                        new[] { "6.9", "(apply <proc> <args>)", "(apply <proc> <arg1> ... <args>)" },
                        (args, env, caller) => ((Procedure)First(args)).Apply(ListStar(Rest(args)), caller, caller), 
                        new ArgsInfo(2, MaxInt, ArgType.Proc, ArgType.Obj))
                .DefinePrimitive(
                        "call-with-current-continuation", 
                        new[] { "6.9", "(call-with-current-continuation <proc>)" },
                        (args, env, caller) => ((Procedure)First(args)).CallCc(caller), 
                        new ArgsInfo(1, ArgType.Proc))
                .DefinePrimitive(
                        "call/cc", 
                        new[] { "(call/cc <proc>)" },
                        (args, env, caller) => ((Procedure)First(args)).CallCc(caller), 
                        new ArgsInfo(1, ArgType.Proc))

                .DefinePrimitive(
                        "force", 
                        new[] { "6.9", "(force <promise>)" },
                        (args, env, caller) => Force((Procedure)First(args), caller), 
                        new ArgsInfo(1, ArgType.Proc))
                ////  Note: list(s) are optional and may be empty lists.
                .DefinePrimitive(
                        "for-each", 
                        new[] { "6.9", "(for-each <proc> <list1> <list2> ...)" },
                        (args, env, caller) => EvaluateMap.Call((Procedure)First(args), Rest(args), false, caller.Env, caller), 
                        new ArgsInfo(1, MaxInt, ArgType.Proc, ArgType.PairOrEmpty))
                ////  Note: list(s) are optional and may be empty lists.
                .DefinePrimitive(
                        "map", 
                        new[] { "6.9", "(map <proc> <list1> <list2> ...)" },
                        (args, env, caller) => EvaluateMap.Call((Procedure)First(args), Rest(args), true, caller.Env, caller), 
                        new ArgsInfo(1, MaxInt, ArgType.Proc, ArgType.PairOrEmpty))
                .DefinePrimitive(
                        "procedure?", 
                        new[] { "6.9", "(procedure? <obj>)" },
                        (args, env, caller) => SchemeBoolean.Truth(First(args) is Procedure), 
                        new ArgsInfo(1, ArgType.Obj));
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Ensure that the given object is a procedure.
        /// Used for checking before attempting to Apply a computed value.
        /// </summary>
        /// <param name="obj">The object to check</param>
        /// <returns>The object as a procedure.</returns>
        internal static Procedure EnsureProcedure(SchemeObject obj)
        {
            Contract.Requires(obj != null);
            if (!(obj is Procedure))
            {
                ErrorHandlers.ProcError("Attempt to use as a procedure", obj);
            }

            return (Procedure)obj;
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// All subclasses have to be able to apply the procedure to arguments.
        /// That is what it means to be a procedure.
        /// The application environment is supplied by the specific procedure subclass, if necessary.
        /// </summary>
        /// <param name="args">The arguments to the procedure, which may or may not have 
        ///   been evaluated.</param>
        /// <param name="returnTo">The evaluator to return to.  This can be different from caller if this is the last step in evaluation</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next evaluator to run after the application.</returns>
        internal abstract Evaluator Apply(SchemeObject args, Evaluator returnTo, Evaluator caller);

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
        internal virtual Evaluator Evaluate(SchemeObject args, Environment env, Evaluator caller)
        {
            Contract.Requires(args != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return EvaluateProc.Call(this, args, env, caller);
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Describe a procedure by returning its value.
        /// </summary>
        /// <returns>The procedure as a string.</returns>
        internal override string Describe()
        {
            return this.ToString(false);
        }
        #endregion

        #region Protected Methods
        /// <summary>
        /// Check the number of args passed.
        /// </summary>
        /// <param name="numArgs">The number of arguments expected.</param>
        /// <param name="args">The arguments passed to the procedure.</param>
        /// <param name="evaluatorName">Name, for the error message.</param>
        /// <param name="caller">The calling evaluator</param>
        protected void CheckArgCount(int numArgs, SchemeObject args, string evaluatorName, Evaluator caller)
        {
            Contract.Requires(args != null);
            Contract.Requires(evaluatorName != null);
            Contract.Requires(caller != null);
            if (numArgs < this.minArgs || numArgs > this.maxArgs)
            {
                this.ArgCountError(numArgs, minArgs, args, evaluatorName, caller);
            }
        }

        /// <summary>
        /// Generate an error when the argument count check fails.
        /// This is also called by CLR procedures, which check their arguments differently.
        /// </summary>
        /// <param name="numArgs">The actual number of arguments.</param>
        /// <param name="expectedArgs">The expected number of arguments.</param>
        /// <param name="args">The arguments actually passed.</param>
        /// <param name="evaluatorName">The name of the evaluator that is checking the count.</param>
        /// <param name="caller">The calling evaluator.</param>
        protected void ArgCountError(int numArgs, int expectedArgs, SchemeObject args, string evaluatorName, Evaluator caller)
        {
            Contract.Requires(args != null);
            Contract.Requires(evaluatorName != null);
            Contract.Requires(caller != null);
            string msg = numArgs < expectedArgs ? "few" : "many";
            int lineNumber = caller.FindLineNumberInCallStack();
            string lineMsg = lineNumber == 0 ? string.Empty : " at line: " + lineNumber;
            ErrorHandlers.SemanticError(
                string.Format(
                    @"""{0}"" too {1} args ({2}) for {3}: ""{4}""{5}",
                    evaluatorName,
                    msg,
                    numArgs,
                    this.ProcedureName,
                    args,
                    lineMsg),
                null);
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
            Contract.Requires(promise != null);
            Contract.Requires(caller != null);
            return promise.Apply(Undefined.Instance, caller, caller);
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
            Contract.Requires(caller != null);
            return this.Apply(MakeList(Continuation.New(caller)), caller, caller);
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.procedureName != null);
        }
        #endregion
    }

    #region ProcedureContract
    /// <summary>
    /// Defined so that we can put preconditions on the abstract function.
    /// </summary>
    [ContractClassFor(typeof(Procedure))]
    internal abstract class ProcedureContract : Procedure
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="ProcedureContract"/> class.
        /// </summary>
        /// <param name="name">The name.</param>
        /// <param name="argsInfo">The args info.</param>
        protected ProcedureContract(string name, ArgsInfo argsInfo)
            : base(name, argsInfo)
        {
        }

        /// <summary>
        /// Apply the procedure to the args.
        /// </summary>
        /// <param name="args">The procedure arguments.</param>
        /// <param name="returnTo">When done, return control here.</param>
        /// <param name="caller">The caller.</param>
        /// <returns>An evaluator to continue the computation.</returns>
        internal override Evaluator Apply(SchemeObject args,  Evaluator returnTo, Evaluator caller)
        {
            Contract.Requires(args != null);
            Contract.Requires(returnTo != null);
            Contract.Requires(caller != null);
            return null;
        }
    }
    #endregion
}