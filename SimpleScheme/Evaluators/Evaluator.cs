// <copyright file="Evaluator.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using System.Threading;

    // ReSharper disable ConvertToAutoProperty

    #region Delegates
    /// <summary>
    /// Each step takes an Evaluator and returns another one.
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
        #region Fields
        /// <summary>
        /// The program counter.
        /// Contains the function to execute next.
        /// This is the type for the evaluator functions.
        /// It takes an Evaluator and returns another Evaluator.
        /// These values are assigned to the pc.
        /// Evaluators must be a <b>static</b> functions only.  This is because if an evaluator
        ///  *instance* is bound to the evaluator, then it would not clone properly, so continuations
        ///   would not work.
        /// </summary>
        private Stepper pc;

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
        /// The evaluation environment.
        /// </summary>
        private Environment env;

        /// <summary>
        /// The calling evaluator. 
        /// Control returns here after evaluation is done.
        /// </summary>
        private Evaluator caller;

        /// <summary>
        /// The evaluation result.
        /// </summary>
        private SchemeObject returnedExpr;

        /// <summary>
        /// The returned environment.  Some evaluators change the
        ///   environment, but this is not common.
        /// </summary>
        private Environment returnedEnv;

        /// <summary>
        /// The type of return (synchronous or asynchronous).
        /// </summary>
        private ReturnType returnFlag;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Evaluator class.
        /// This class is not instantiated itself, but only derived classes.
        /// </summary>
        /// <param name="initialPc">The initial pc value.</param>
        /// <param name="args">The expression to evaluate.</param>
        /// <param name="env">The evaluator environment.</param>
        /// <param name="caller">The caller evaluator.</param>
        /// <param name="counterId">The counter ID associated with this evaluator.</param>
        internal protected Evaluator(Stepper initialPc, SchemeObject args, Environment env, Evaluator caller, int counterId)
        {
            this.expr = args;
            this.env = env;
            this.caller = caller;
            this.pc = initialPc;
            this.lineNumber = 0;
            this.returnedExpr = Undefined.Instance;
            this.returnFlag = ReturnType.SynchronousReturn;
            this.caught = 0;
#if Diagnostics
            this.IncrementCounter(counterId);
#endif
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets or sets the current program counter.
        /// </summary>
        internal Stepper Pc
        {
            get { return this.pc; }
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
        /// Every evaluator has a copy of the interpreter, so we don't have to search down the
        ///   chain for it.
        /// This never changes, even if Env does get updated.
        /// </summary>
        internal Interpreter Interp
        {
            get { return this.Env.Interp; }
        }

        /// <summary>
        /// Gets or sets the expression being evaluated.
        /// </summary>
        internal SchemeObject Expr
        {
            get { return this.expr; }
            set { this.expr = value; }
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
        /// Gets the evaluation environment.  
        /// After execution concludes, this is the new environment.
        /// </summary>
        internal Environment Env
        {
            get { return this.env; }
        }

        /// <summary>
        /// Gets the caller of this evaluator.
        /// </summary>
        internal Evaluator Caller
        {
            get { return this.caller; }
        }

        /// <summary>
        /// Gets or sets the returned expression from the last call.
        /// </summary>
        internal SchemeObject ReturnedExpr
        {
            get { return this.returnedExpr; }
            set { this.returnedExpr = value; }
        }

        /// <summary>
        /// Gets or sets the returned environment from the last call.
        /// Most primitives do not change the environment, but some do.
        /// </summary>
        internal Environment ReturnedEnv
        {
            get { return this.returnedEnv; }
            set { this.returnedEnv = value; }
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

        #region Internal Methods
        /// <summary>
        /// Convert an obj into a string representation.
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
            while (s.Caller != null)
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
            while (step != null)
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
            if (this.Env != null)
            {
                this.Env.Interp.IncrementCounter(counterIdent);
            }
        }

        /// <summary>
        /// Call the interpreter in the environment to start evaluating steps.
        /// </summary>
        /// <returns>The return value of the evaluation (or halted or suspended).</returns>
        internal SchemeObject EvalStep()
        {
            return this.Env.Interp.EvalSteps(this);
        }

        #endregion

        #region Internal Virtual Methods
        /// <summary>
        /// Give the Evaluator to execute the next step.
        /// Normally, we just continue on the one we are in by returning this.
        /// The halt and suspend evaluators override this.
        /// Return null to stop evaluation, and leave result in ReturnedExpr.
        /// Return a different step to jump to that step.
        /// </summary>
        /// <returns>Null to break out of main loop, a different evaluator to jump to it.</returns>
        internal virtual Evaluator NextStep()
        {
            return this;
        }

        /// <summary>
        /// Perform a shallow copy of the evaluator.
        /// Subclass overrides this to provide specialized implementation.
        /// </summary>
        /// <returns>A copy of the evaluator.</returns>
        internal virtual Evaluator Clone()
        {
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
        /// <param name="flag">The return type.</param>
        internal virtual void UpdateReturnFlag(ReturnType flag)
        {
            this.returnFlag = flag;
        }
        #endregion

        #region Protected Methods
        /// <summary>
        /// Update and environment.
        /// Formals must be a list of symbols, vals is a corresponding list of nver values.
        /// Update the value of each of the symbols in the environment.
        /// </summary>
        /// <param name="formals">A list of symbols.</param>
        /// <param name="vals">A corresponding list of values.</param>
        protected void UpdateEnvironment(SchemeObject formals, SchemeObject vals)
        {
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

        #region Private Methods
        /// <summary>
        /// Dump the current evaluator into a string builder.
        /// </summary>
        /// <param name="buf">The string builder to write to.</param>#
        private void DumpStep(StringBuilder buf)
        {
            buf.AppendFormat("Exaluator {0}\n", this.SchemeTypeName());
            string exp = this.Expr is EmptyList ? "()" : this.Expr.ToString();
            buf.AppendFormat("  Expr: {0}\n", exp);
            if (this.Env != null)
            {
                buf.AppendFormat("  Env:\n{0}", this.Env.Dump(1, 3));
            }
        }
        #endregion
    }
}
