// <copyright file="Evaluator.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;
    using System.Collections.Concurrent;
    using System.Diagnostics.CodeAnalysis;
    using System.Diagnostics.Contracts;
    using System.Reflection;
    using System.Text;
    using System.Threading;

    // ReSharper disable ConvertToAutoProperty
    #region Delegates
    /// <summary>
    /// Each step takes an Evaluator.  Actually an open delegate, used to call an instance method.
    /// </summary>
    /// <param name="curr">The current step.</param>
    /// <returns>The next step to execute.</returns>
    public delegate Evaluator Stepper(Evaluator curr);
    #endregion

    #region Enums
    /// <summary>
    /// Codes to pass back to tell the caller what happened.
    /// </summary>
    internal enum ReturnType
    {
        /// <summary>
        /// The normal synchronous return.  Has ReturnedExpr.
        /// </summary>
        SynchronousReturn,

        /// <summary>
        /// The evaluation suspended due to async call.  No ReturnedExpr.
        /// </summary>
        CaughtSuspended,

        /// <summary>
        /// The evaluation returned a value after previously suspending.
        /// Has ReturnedExpr.
        /// </summary>
        AsynchronousReturn
    }
    #endregion

    /// <summary>
    /// Evaluates expressions step by step.
    /// Base class for all other evaluators.
    /// </summary>
    public abstract class Evaluator : EvaluatorOrObject
    {
        #region Static Fields
        /// <summary>
        /// Free list of evaluators.  Used to cut down on garbage collection.
        /// </summary>
        protected static readonly Dictionary<Type, ConcurrentStack<Evaluator>> FreeList = new Dictionary<Type, ConcurrentStack<Evaluator>>();

        /// <summary>
        /// This maps OpCode values to the corresponding virtual methods.
        /// These are open delegates (instance methods not bound to a specific instance) so the
        /// caller can supply the correct Evaluator instance.
        /// </summary>
        private static readonly Stepper[] Instructions;
        #endregion

        #region Fields
        /// <summary>
        /// Flag indicates a degenerate evaluator.
        /// The only degenerate evaluator is the FinalEvaluator.
        /// </summary>
        protected bool degenerate;

        /// <summary>
        /// The current interpreter.
        /// Copied to every evaluator for ease of access.
        /// </summary>
        private Interpreter interp;

        /// <summary>
        /// The program counter.
        /// Contains an enum value specifying the the virtual method to execute next.
        /// Should always be set before returning from a step.
        /// </summary>
        private OpCode pc;

        /// <summary>
        /// The evaluation environment.
        /// </summary>
        private Environment env;

        /// <summary>
        /// The number of asynchronous evaluations that are waiting to complete.
        /// </summary>
        private int caught;

        /// <summary>
        /// The expression being evaluated.  
        /// This can change during the course of evaluation.
        /// </summary>
        private SchemeObject expr;

        /// <summary>
        /// The line number where evaluation is taking place.
        /// </summary>
        private int lineNumber;

        /// <summary>
        /// The calling evaluator. 
        /// Control returns here after evaluation is done.
        /// This would be readonly, except for the clone operation.
        /// </summary>
        private Evaluator caller;

        /// <summary>
        /// The evaluation result.
        /// This is filled when the evaluator is finished.
        /// The caller can retrieve the returned expr as long is it retains the reference.
        /// </summary>
        private SchemeObject returnedExpr;

        /// <summary>
        /// The type of return (synchronous or asynchronous).
        /// </summary>
        private ReturnType returnFlag;

        private static int evalCount = 0;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes static members of the <see cref="Evaluator"/> class. 
        /// This sets up the map of OpCode to Stepper.
        /// </summary>
        static Evaluator()
        {
            Instructions = new Stepper[(int)OpCode.Illegal + 1];
            Instructions[(int)OpCode.Initial] = GetStepper("InitialStep");
            Instructions[(int)OpCode.Done] = GetStepper("DoneStep");
            Instructions[(int)OpCode.Test] = GetStepper("TestStep");
            Instructions[(int)OpCode.Iterate] = GetStepper("IterateStep");
            Instructions[(int)OpCode.Loop] = GetStepper("LoopStep");
            Instructions[(int)OpCode.Evaluate] = GetStepper("EvaluateStep");
            Instructions[(int)OpCode.End] = GetStepper("EndStep");
            Instructions[(int)OpCode.EvalTest] = GetStepper("EvalTestStep");
            Instructions[(int)OpCode.EvalExpr] = GetStepper("EvalExprStep");
            Instructions[(int)OpCode.EvalAlternative] = GetStepper("EvalAlternativeStep");
            Instructions[(int)OpCode.EvalKey] = GetStepper("EvalKeyStep");
            Instructions[(int)OpCode.CheckClause] = GetStepper("CheckClauseStep");
            Instructions[(int)OpCode.EvalClause] = GetStepper("EvalClauseStep");
            Instructions[(int)OpCode.EvalConsequent] = GetStepper("EvalConsequentStep");
            Instructions[(int)OpCode.ApplyRecipient] = GetStepper("ApplyRecipientStep");
            Instructions[(int)OpCode.StoreDefine] = GetStepper("StoreDefineStep");
            Instructions[(int)OpCode.Expand] = GetStepper("ExpandStep");
            Instructions[(int)OpCode.EvalInit] = GetStepper("EvalInitStep");
            Instructions[(int)OpCode.BindVarToInit] = GetStepper("BindVarToInitStep");
            Instructions[(int)OpCode.Apply] = GetStepper("ApplyStep");
            Instructions[(int)OpCode.ApplyProc] = GetStepper("ApplyProcStep");
            Instructions[(int)OpCode.ApplyNamedLet] = GetStepper("ApplyNamedLetStep");
            Instructions[(int)OpCode.ApplyFun] = GetStepper("ApplyFunStep");
            Instructions[(int)OpCode.CollectAndLoop] = GetStepper("CollectAndLoopStep");
            Instructions[(int)OpCode.Set] = GetStepper("SetStep");
            Instructions[(int)OpCode.Close] = GetStepper("CloseStep");
            Instructions[(int)OpCode.ContinueAfterSuspended] = GetStepper("ContinueAfterSuspendedStep");
            Instructions[(int)OpCode.Illegal] = GetStepper("IllegalStep");

            FreeList.Add(typeof(EvaluateAnd), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateCallWithInputFile), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateCallWithOutputFile), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateCase), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateCond), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateDefine), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateDo), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateExpandMacro), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateExpression), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateExpressionWithCatch), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateIf), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateLet), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateLetRec), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateLetStar), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateList), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateMap), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateOr), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateParallel), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateProc), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateSequence), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateSet), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateTime), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(EvaluateTimeCall), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(FinalEvaluator), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(HaltedEvaluator), new ConcurrentStack<Evaluator>());
            FreeList.Add(typeof(SuspendedEvaluator), new ConcurrentStack<Evaluator>());
        }

        /// <summary>
        /// Initializes a new instance of the Evaluator class.
        /// For use by FinalEvaluator only.
        /// </summary>
        protected internal Evaluator()
        {
            this.Initialize();
            Console.WriteLine("Construct " + this.GetType().Name + " " + evalCount++);
        }

        protected Evaluator Initialize()
        {
            this.pc = OpCode.Illegal;
            this.degenerate = true;
            return this;
        }

        /// <summary>
        /// Initialize the member variables.
        /// </summary>
        /// <param name="initialPc">The initial pc value.</param>
        /// <param name="exp">The expression to evaluate.</param>
        /// <param name="envir">The evaluator environment.</param>
        /// <param name="returnTo">The caller evaluator.</param>
        /// <param name="counterId">The counter ID associated with this evaluator.</param>
        protected Evaluator Initialize(OpCode initialPc, SchemeObject exp, Environment envir, Evaluator returnTo, int counterId)
        {
            Contract.Requires(exp != null);
            Contract.Requires(envir != null);
            Contract.Requires(returnTo != null);
            this.expr = exp;
            this.env = envir;
            this.caller = returnTo;
            this.interp = env.InterpOrNull;
            this.pc = initialPc;
            this.returnedExpr = Undefined.Instance;
            this.returnFlag = ReturnType.SynchronousReturn;
            this.caught = 0;
            this.degenerate = false;
#if Diagnostics
            this.IncrementCounter(counterId);
#endif
            return this;
        }
        #endregion

        #region Enums
        /// <summary>
        /// Every Evaluator consists of a set of steps.
        /// The step is implemented as a virtual method.  Each of the OpCode 
        /// values correspoind to one of these virtual methods.
        /// The basic interpreter loop (Interpreter.RunSteps) consists of getting the
        /// Evaluator's Pc, using that to select the corresponding virtual method, and calling
        /// that method.  
        /// Each step (1) updates it own Pc to point to the next step it should execute, and
        /// (2) returns the next Evaluator to run.
        /// A "call" is accomplished by instantiating a new evaluator and returning it.
        /// A "return" is accomplished by returning a saved evaluator.
        /// </summary>
        [SuppressMessage("StyleCop.CSharp.DocumentationRules", "SA1602:EnumerationItemsMustBeDocumented", Justification = "No value here.")]
        #pragma warning disable 1591
        public enum OpCode
        {
            Initial, Done, Test, Iterate, Loop, Evaluate, End, EvalTest, EvalExpr,
            EvalAlternative, EvalKey, CheckClause, EvalClause, EvalConsequent, ApplyRecipient, 
            StoreDefine, Expand, EvalInit, BindVarToInit, Apply, ApplyProc, ApplyNamedLet,
            ApplyFun, CollectAndLoop, Set, Close, ContinueAfterSuspended,
            Illegal
        }
        #pragma warning restore 1591
        #endregion

        #region Accessors
        /// <summary>
        /// Sets the current program counter.
        /// </summary>
        internal OpCode Pc
        {
            set { this.pc = value; }
        }

        /// <summary>
        /// Gets a value indicating whether the evaluator is suspended.
        /// </summary>
        internal bool IsSuspended
        {
            get { return this is SuspendedEvaluator; }
        }

        /// <summary>
        /// Gets the interpreter.
        /// This contains the global interpretation state, such as the current ports, trace flags,
        ///   and counters.
        /// Every evaluator has a reference to the interpreter, so we don't have to search down the
        ///   chain for it.
        /// This never changes, even if Env gets updated.
        /// </summary>
        internal Interpreter Interp
        {
            get
            {
                Contract.Ensures(Contract.Result<Interpreter>() != null);
#if Check
                if (this.interp == null)
                {
                    ErrorHandlers.InternalError("Attempt to access null interpreter");
                }
#endif

                return this.interp;
            }
        }

        /// <summary>
        /// Gets or sets the expression being evaluated.
        /// </summary>
        internal SchemeObject Expr
        {
            get
            {
                Contract.Ensures(Contract.Result<SchemeObject>() != null);
#if Check
                if (this.degenerate)
                {
                    ErrorHandlers.InternalError("Bad evaluator: expr");
                }
#endif
                return this.expr;
            }

            set
            {
                Contract.Requires(value != null);
                this.expr = value;
            }
        }

        /// <summary>
        /// Gets or sets the line number.
        /// </summary>
        internal int LineNumber
        {
            get { return this.lineNumber; }
            set { this.lineNumber = value; }
        }

        /// <summary>
        /// Gets or sets the evaluation environment.  
        /// The only place that this is set is in Continuation, which is the only
        /// case where environment is replaced by something completely different.
        /// After execution concludes, this is the new environment.
        /// </summary>
        internal Environment Env
        {
            get
            {
                Contract.Ensures(Contract.Result<Environment>() != null);
#if Check
                if (this.degenerate)
                {
                    ErrorHandlers.InternalError("Bad evaluator: env");
                }
#endif
                return this.env;
            }

            set
            {
                Contract.Requires(value != null);
                this.env = value;
            }
        }

        /// <summary>
        /// Gets the caller of this evaluator.
        /// </summary>
        internal Evaluator Caller
        {
            get
            {
                Contract.Ensures(Contract.Result<Evaluator>() != null);
#if Check
                if (this.degenerate)
                {
                    ErrorHandlers.InternalError("Bad evaluator: caller");
                }
#endif
                return this.caller;
            }
        }

        /// <summary>
        /// Gets or sets the returned expression from the last call.
        /// </summary>
        internal SchemeObject ReturnedExpr
        {
            get
            {
                Contract.Ensures(Contract.Result<SchemeObject>() != null);
#if Check
                if (this.returnedExpr == null)
                {
                    ErrorHandlers.InternalError("Bad evaluator: returnedExpr");
                }
#endif
                return this.returnedExpr;
            }

            set
            {
                Contract.Requires(value != null);
                this.returnedExpr = value;
            }
        }

        /// <summary>
        /// Gets a flag indicating whether the return is synchronous, asynchronous, or catch.
        /// </summary>
        internal ReturnType ReturnFlag
        {
            get { return this.returnFlag; }
        }
        #endregion

        #region Virtual Accessors
        /// <summary>
        /// Gets a value indicating whether to catch suspended execution.
        /// </summary>
        /// <returns>By default, do not catch.</returns>
        internal virtual bool CatchSuspended
        {
            get { return false; }
        }
        #endregion

        #region Main Evaluator Dispatch Method
        /// <summary>
        /// Gets the instruction at the current program counter, looks up the
        /// stepper method, and calls it.  The method is an open delegate to a virtual
        /// method, so even though it is called with one argument, it actually
        /// invokes a member method with no arguments.
        /// </summary>
        internal Evaluator Step()
        {
            return Instructions[(int)this.pc](this);
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Convert an obj into a string representation.
        /// This is normally used only on SchemeObject
        /// </summary>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <returns>The string representing the obj.</returns>
        internal override string ToString(bool quoted)
        {
            return "<evaluator>";
        }

        /// <summary>
        /// Increment the caught counter.
        /// </summary>
        /// <returns>The new value of the caught flag.</returns>
        internal int IncrementCaught()
        {
            Interlocked.Increment(ref this.caught);
            return this.caught;
        }

        /// <summary>
        /// Makes a copy of the whole chain of evaluation steps, from the current evaluator back to
        ///   the original evaluator.  This chain is saved as the continuation.  
        /// The cloned chain is linked up with each other to form a new chain.
        /// Each subclass makes a copy of its own member variables.
        /// This is needed to support continuations, because the evaluator contains mutable state.  If the state 
        ///  is changed after the saving of the continuation, then the continuation would not represent the
        ///  correct state.  
        /// For this shallow copy to be correct, each evaluator must modify ONLY its own variables, not the
        ///  things to which they point.
        /// </summary>
        /// <returns>A copy of the current evaluator.</returns>
        internal Evaluator CloneChain()
        {
            Evaluator ret = this.Clone();
            Evaluator s = ret;
            while (!(s.Caller is FinalEvaluator))
            {
                Evaluator parent = s.Caller.Clone();
                s.caller = parent;
                s = s.Caller;
            }

            return ret;
        }

        /// <summary>
        /// Create a stack backtrace
        /// </summary>
        /// <returns>A backtrace of the evaluator call stack.</returns>
        internal string StackBacktrace()
        {
            Contract.Ensures(Contract.Result<string>() != null);
            Evaluator step = this.Caller;    // skip backtrace evaluator itself
            var sb = new StringBuilder();
            while (step != null)
            {
                step.DumpStep(sb);
                step = step.Caller;
            }

            return sb.ToString();
        }

        /// <summary>
        /// Look up the evaluation stack for an evaluator with a line number.
        /// </summary>
        /// <returns>The line number, or 0 if none found.</returns>
        internal int FindLineNumberInCallStack()
        {
            Evaluator step = this;
            while (!(step is FinalEvaluator))
            {
                if (step.LineNumber != 0)
                {
                    return step.LineNumber;
                }

                step = step.Caller;
            }

            return 0;
        }

        /// <summary>
        /// Increment the given counter.
        /// </summary>
        /// <param name="counterIdent">The counter id</param>
        internal void IncrementCounter(int counterIdent)
        {
            Contract.Requires(counterIdent >= 0);
            if (this.env != null)
            {
                this.Env.IncrementCounter(counterIdent);
            }
        }
        #endregion

        #region Internal Virtual Methods
        /// <summary>
        /// When the evaluator is no longer needed, reclaim it for reuse.
        /// </summary>
        protected void Reclaim()
        {
            PutInstance(this);
        }

        /// <summary>
        /// Perform a shallow copy of the evaluator.
        /// Subclass overrides this to provide specialized implementation.
        /// </summary>
        /// <returns>A copy of the evaluator.</returns>
        internal virtual Evaluator Clone()
        {
            Contract.Ensures(Contract.Result<Evaluator>() != null);
            return (Evaluator)this.MemberwiseClone();
        }

        /// <summary>
        /// Trace information for the evaluator.
        /// Subclass can override if it wants to supply trace information.
        /// </summary>
        /// <returns>Info to print for the trace.</returns>
        internal virtual string TraceInfo()
        {
            return null;
        }
        
        // TODO can we NOT make this virtual ??

        /// <summary>
        /// Stores the return type.
        /// </summary>
        /// <param name="exp">The returned expression (unused).</param>
        /// <param name="flag">The return type.</param>
        internal virtual void UpdateReturnFlag(SchemeObject exp, ReturnType flag)
        {
            this.returnFlag = flag;
        }
        #endregion

        #region Protected Methods
        /// <summary>
        /// Get an instance of en avaluator of type T.
        /// If there is one on the free list, then return it.
        /// Otherwise, create a new one.
        /// Don't bother to initialize it -- that will be done by the caller.
        /// </summary>
        /// <typeparam name="T">The type of the evaluator.</typeparam>
        /// <returns>An instance of the given eavulator type.</returns>
        protected static T GetInstance<T>() where T : Evaluator
        {
            Evaluator eval;
            if (FreeList[typeof(T)].TryPop(out eval))
            {
                return (T)eval;
            }

            return Activator.CreateInstance<T>();
        }

        /// <summary>
        /// Put an evaluator instance on the appropriate free list.
        /// The ealuator can be reused immediately after, so make sure that the caller does not need it any longer.
        /// </summary>
        /// <param name="ev">The evaluator to place on a free list.</param>
        protected static void PutInstance(Evaluator ev)
        {
            FreeList[ev.GetType()].Push(ev);
        }

        /// <summary>
        /// To return from an evaluator, a step needs to assign a value to the
        /// ReturnedExpr and supply the calling evaluator as the next step.
        /// </summary>
        /// <param name="res">The evaluation result.</param>
        /// <returns>The next evaluator.</returns>
        protected Evaluator ReturnFromEvaluator(SchemeObject res)
        {
            Contract.Assert(this.caller != null);
            this.caller.ReturnedExpr = res;
            var returnTo = this.caller;     // copy before reclaiming
            this.Reclaim();
            return returnTo;
        }

        /// <summary>
        /// Update and environment.
        /// Formals must be a list of symbols, vals is a corresponding list of nver values.
        /// Update the value of each of the symbols in the environment.
        /// </summary>
        /// <param name="formals">A list of symbols.</param>
        /// <param name="vals">A corresponding list of values.</param>
        protected void UpdateEnvironment(SchemeObject formals, SchemeObject vals)
        {
            Contract.Requires(formals != null);
            Contract.Requires(vals != null);
            while (formals is Pair)
            {
                this.env.Update(First(formals), First(vals));
                formals = Rest(formals);
                vals = Rest(vals);
            }
        }

        /// <summary>
        /// Return the caught counter and set it to zero in an atomic operation.
        /// </summary>
        /// <returns>The new value of the caught flag.</returns>
        protected int FetchAndResetCaught()
        {
            return Interlocked.Exchange(ref this.caught, 0);
        }
        #endregion

        #region Step Virtual Methods
        // These virtual methods are used by subclasses as their internal steps.
        // The subclass assigns the PC to one of its member functions.
        // The delegate MUST BE and *open* instance method, not bound to a specific
        // instance, otherwise call/cc will not work as expected.  The caller, Interpreter:RunSteps,
        // supplies the instance explicitly in the call.

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator InitialStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: InitialStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator DoneStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: DoneStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator TestStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: TestStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator IterateStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: IterateStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator LoopStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: LoopStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator EvaluateStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: EvaluateStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator EndStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: InitialStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator EvalTestStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: EvalTestStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator EvalExprStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: EvalExprStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator EvalAlternativeStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: EvalAlternativeStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator EvalKeyStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: EvalKeyStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator CheckClauseStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: CheckClauseStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator EvalClauseStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: EvalClauseStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator EvalConsequentStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: EvalConsequentStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator ApplyRecipientStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: ApplyRecipientStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator StoreDefineStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: StoreDefineStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator ExpandStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: ExpandStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator EvalInitStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: EvalInitStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator BindVarToInitStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: BindVarToInitStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator ApplyStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: ApplyStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator ApplyProcStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: ApplyProcStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator ApplyNamedLetStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: ApplyNamedLetStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator ApplyFunStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: ApplyFunStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator CollectAndLoopStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: CollectAndLoopStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator SetStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: SetStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator CloseStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: CloseStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator ContinueAfterSuspendedStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: ContinueAfterSuspendedStep");
            return null;
        }

        /// <summary>
        /// Base declaration for step.  Should never be called.
        /// </summary>
        protected virtual Evaluator IllegalStep()
        {
            ErrorHandlers.InternalError("Bad step in evaluator: IllegalStep");
            return null;
        }
        #endregion

        #region Private Static
        /// <summary>
        /// Gets a stepper function.
        /// This gets an open instance method, which is bound to an actual
        /// instance at the point of the call.
        /// </summary>
        /// <param name="methodName">
        /// The name of the method to call.  It must be an instance method of Evaluator.
        /// In practice it is local to the evaluator subclass, so the method must be virtual.
        /// </param>
        /// <returns>A stepper that can be used to call the method.</returns>
        private static Stepper GetStepper(string methodName)
        {
            Contract.Requires(methodName != null);
            Contract.Ensures(Contract.Result<Stepper>() != null);
            MethodInfo mi = typeof(Evaluator).GetMethod(methodName, BindingFlags.NonPublic | BindingFlags.Instance);
            if (mi == null)
            {
                ErrorHandlers.InternalError("Could not find step: " + methodName);
                return null;
            }

            return (Stepper)Delegate.CreateDelegate(typeof(Stepper), null, mi);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Dump the current evaluator into a string builder.
        /// </summary>
        /// <param name="buf">The string builder to write to.</param>#
        private void DumpStep(StringBuilder buf)
        {
            Contract.Requires(buf != null);
            Contract.Assert(this.Expr != null);
            Contract.Assert(this.Env != null);
            buf.AppendFormat("Exaluator {0}\n", this.SchemeTypeName());
            string exp = this.Expr is EmptyList ? "()" : this.Expr.ToString();
            buf.AppendFormat("  Expr: {0}\n", exp);
            if (this.env != null)
            {
                buf.AppendFormat("  Env:\n{0}", this.Env.Dump(1, 3));
            }
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.degenerate || this.expr != null);
            Contract.Invariant(this.degenerate || this.env != null);
            Contract.Invariant(this.degenerate || this.caller != null);
            Contract.Invariant(this.degenerate || this.returnedExpr != null);
        }
        #endregion
    }
}
