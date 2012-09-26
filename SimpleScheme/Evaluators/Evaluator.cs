// <copyright file="Evaluator.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Text;
    using System.Threading;
    using Obj = System.Object;

    /// <summary>
    /// Evaluates expressions step by step.
    /// Base class for all other evaluators.
    /// </summary>
    public class Evaluator : Printable
    {
        #region Constants
        /// <summary>
        /// The printable name of the evaluator type.
        /// </summary>
        public const string Name = "evaluator";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("step");
        #endregion

        #region Fields

        /// <summary>
        /// The program counter.
        /// Contains the function to execute next.
        /// This is the type for the evaluator functions.
        /// It takes no arguments and returns a Evaluator.
        /// These values are assigned to the pc.
        /// Evaluators must be static functions only.  This is because if an evaluator
        ///   instance is bound to the evaluator, then it could not clone properly, so continuations
        ///   would not work.
        /// </summary>
        private Func<Evaluator, Evaluator> pc;

        /// <summary>
        /// The number of asynchronous evaluations that are waiting to complete.
        /// </summary>
        private int caught;

        /// <summary>
        /// Indicates whether a trace has been performed on this evaluator instance.
        /// </summary>
        private bool traced;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Evaluator class.
        /// This class is not instantiated itself, but only derived classes.
        /// </summary>
        /// <param name="args">The expression to evaluate.</param>
        /// <param name="env">The evaluator environment.</param>
        /// <param name="caller">The caller evaluator.</param>
        protected Evaluator(Obj args, Environment env, Evaluator caller)
        {
            this.CallerCaller = caller;
            this.Expr = args;
            this.Env = env;
            this.ReturnedExpr = Undefined.New();
            this.ReturnFlag = ReturnType.SynchronousReturn;
            this.caught = 0;
            this.traced = false;
        }
        #endregion

        #region Enums
        /// <summary>
        /// Codes to pass back to tell the caller what happened.
        /// </summary>
        public enum ReturnType
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

        #region Accessors
        /// <summary>
        /// Gets a value indicating whether the evaluator is suspended.
        /// </summary>
        public bool IsSuspendedEvaluator
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
        public Interpreter Interp
        {
            get { return this.Env.Interp; }
        }

        /// <summary>
        /// Gets the expression being evaluated.
        /// </summary>
        public object Expr { get; private set; }

        /// <summary>
        /// Gets the returned expression from the last call.
        /// </summary>
        public object ReturnedExpr { get; private set; }

        /// <summary>
        /// Gets the evaluation environment.  After execution, this is the new environment.
        /// </summary>
        public Environment Env { get; private set; }

        /// <summary>
        /// Gets the returned environment from the last call.
        /// Most primitives do not change the environment, but some do.
        /// </summary>
        public Environment ReturnedEnv { get; private set; }

        /// <summary>
        /// Gets the caller of this evaluator.
        /// Immutable.
        /// </summary>
        public Evaluator Caller
        {
            get { return this.CallerCaller; }
        }

        /// <summary>
        /// Gets the caller's caller.
        /// </summary>
        public Evaluator CallerCaller { get; private set; }

        /// <summary>
        /// Gets a flag indicating whether the return is synchronous, asynchronous, or catch.
        /// </summary>
        public ReturnType ReturnFlag { get; private set; }

        /// <summary>
        /// Gets a value indicating whether to catch suspended execution.
        /// </summary>
        /// <returns>By default, do not catch.</returns>
        public virtual bool CatchSuspended
        {
            get { return false; }
        }

        #endregion


        #region Public Static Methods
        /// <summary>
        /// Transfer to a given evaluator.  
        /// This can be used to return fram an evaluator.
        /// Assign the return value and return the caller task to resume.
        /// The Call/CC handler uses this to transfer to a saved continuation.
        /// </summary>
        /// <param name="nextStep">the evaluator to transfer to.</param>
        /// <param name="expr">The value to save as the returned value.</param>
        /// <param name="env">The environment to save as the returned environment.</param>
        /// <returns>The next evaluator.  This is in the caller for return.</returns>
        public static Evaluator TransferToStep(Evaluator nextStep, Obj expr, Environment env)
        {
            nextStep.ReturnedExpr = expr;
            nextStep.ReturnedEnv = env;
            return nextStep;
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write the evaluator to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public void PrintString(bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append("<" + Name + ">");
            }
        }

        /// <summary>
        /// Perform a shallow copy of the evaluator.
        /// Subclass overrides this to provide specialized implementation.
        /// </summary>
        /// <returns>A copy of the evaluator.</returns>
        public virtual Evaluator Clone()
        {
            return (Evaluator)this.MemberwiseClone();
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
        public Evaluator CloneChain()
        {
            Evaluator ret = this.Clone();
            Evaluator s = ret;
            while (s.Caller != null)
            {
                Evaluator parent = s.Caller.Clone();
                s.CallerCaller = parent;
                s = s.Caller;
            }

            return ret;
        }

        /// <summary>
        /// Give the evaluator a chance to divert execution.
        /// The halt, suspend, and end evaluators do this.
        /// Return null to break out of evaluation main loop.
        /// Return a different step to jump to that step.
        /// Return this to just proceed as normal.
        /// </summary>
        /// <returns>Null to break out of main loop, a different evaluator to jump to it.</returns>
        public virtual Evaluator Divert()
        {
            return this;
        }

        /// <summary>
        /// When a step is suspended, check with each caller up the chain, seeing if any
        ///   one of them want to resume.
        /// </summary>
        /// <returns>The evaluator that wants to handle suspension, orherwise null</returns>
        public Evaluator SearchForHandler()
        {
            Evaluator step = this;
            while (! (step is HaltedEvaluator))
            {
                if (step.CatchSuspended)
                {
                    return step;
                }

                step = step.Caller;
            }

            return null;
        }

        /// <summary>
        /// Trace information for the evaluator.
        /// Do this only once per instance.
        /// </summary>
        /// <returns>Info to print for the trace.</returns>
        public virtual string TraceInfo()
        {
            if (this.traced)
            {
                return null;
            }

            this.traced = true;
            return TypePrimitives.TypeName(this);
        }

        /// <summary>
        /// Run the step represented by the PC.
        /// Just execute the evaluator function stored in pc.
        /// The instance is passed to the static function, not wrapped in the delegate.
        /// The pc is a delegate, and must not be bound to an instance, only to a static function.
        /// Otherwise, the shallow copy that is used to create a continuation
        ///   will be bound to the wrong evaluator.  Check to make sure this is handled correctly.
        /// </summary>
        /// <returns>The next step to run.</returns>
        public Evaluator RunStep()
        {
            this.TraceStep();
#if DEBUG
            this.IncrementCounter(counter);
            if (this.pc.Target != null)
            {
                ErrorHandlers.InternalError("Step bound to instance: " + this.pc);
            }
#endif

            return this.pc(this);
        }

        /// <summary>
        /// Call the interpreter in the environment to start evaluating steps.
        /// </summary>
        /// <returns>The return value of the evaluation (or halted or suspended).</returns>
        public Obj EvalStep()
        {
            return this.Env.Interp.EvalSteps(this);
        }

        /// <summary>
        /// Create a stack backtrace
        /// </summary>
        /// <returns>A backtrace of the evaluator call stack.</returns>
        public string StackBacktrace()
        {
            Evaluator step = this.Caller;    // skip backtrace itself
            StringBuilder sb = new StringBuilder();
            while (step != null)
            {
                step.DumpStep(sb);
                step = step.Caller;
            }

            return sb.ToString();
        }

        /// <summary>
        /// Increment the given counter.
        /// </summary>
        /// <param name="counterIdent">The counter id</param>
        public void IncrementCounter(int counterIdent)
        {
            if (this.Env != null)
            {
                this.Env.Interp.IncrementCounter(counterIdent);
            }
        }
        #endregion

        #region Protected Methods
        /// <summary>
        /// Create a new environment and replace the current one with it.
        /// The new one has the same lexical parent as it did before.
        /// </summary>
        /// <param name="formals">The environment variable names.</param>
        /// <param name="vals">The values of the variables.</param>
        public void ReplaceEnvironment(Obj formals, Obj vals)
        {
            this.Env = new Environment(formals, vals, this.Env.LexicalParent);
        }

        /// <summary>
        /// Push an empty environment.
        /// </summary>
        /// <param name="parent">The lexically enclosing environment.</param>
        public void PushEmptyEnvironment(Environment parent)
        {
            this.Env = new Environment(parent);
        }

        /// <summary>
        /// Continue executing in the existing evaluator, but set the expr.
        /// </summary>
        /// <param name="exp">The new expr value.</param>
        /// <returns>The next evaluator, which is this evaluator.</returns>
        public Evaluator UpdateExpr(Obj exp)
        {
            this.Expr = exp;
            return this;
        }

        /// <summary>
        /// Continue executing in the existing evaluator, but step
        ///   one element down the list.
        /// If the expression is an empty list, then this is a no-op.
        /// </summary>
        /// <returns>The next evaluator, which is this evaluator.</returns>
        public Evaluator StepDownExpr()
        {
            this.Expr = this.Expr.Rest();
            return this;
        }

        /// <summary>
        /// Increment the caught counter.
        /// </summary>
        /// <returns>The new value of the caught flag.</returns>
        public int IncrementCaught()
        {
            Interlocked.Increment(ref this.caught);
            return this.caught;
        }

        /// <summary>
        /// Return the caught counter and set it to zero in an atomic operation.
        /// </summary>
        /// <returns>The new value of the caught flag.</returns>
        public int FetchAndResetCaught()
        {
            return Interlocked.Exchange(ref this.caught, 0);
        }

        /// <summary>
        /// Continue executing in this evaluator, but set the returned expr.
        /// Usually invoked on an object's caller.  
        /// </summary>
        /// <param name="exp">The returned value.</param>
        /// <returns>The next evaluator, which is this evaluator.</returns>
        public virtual Evaluator UpdateReturnValue(Obj exp)
        {
            this.ReturnedExpr = exp;
            return this;
        }
        
        /// <summary>
        /// Stores the return value and the returned environment.
        /// </summary>
        /// <param name="exp">The return value</param>
        /// <param name="envir">The returned environment.</param>
        /// <returns>The next evaluator.</returns>
        public virtual Evaluator UpdateReturnValue(Obj exp, Environment envir)
        {
            this.ReturnedExpr = exp;
            this.ReturnedEnv = envir;
            return this;
        }

        /// <summary>
        /// Stores the return value and the return type.
        /// </summary>
        /// <param name="exp">The return value.</param>
        /// <param name="flag">The return type.</param>
        /// <returns>The next evaluator.</returns>
        public virtual Evaluator UpdateReturnValue(Obj exp, ReturnType flag)
        {
            this.ReturnedExpr = exp;
            this.ReturnFlag = flag;
            return this;
        }

        /// <summary>
        /// Update the caller field.
        /// Normally this would not be changable, but in the parallel primitive
        ///   it is necessary to suppress the normal return and to make the
        ///   evaluation halt.
        /// </summary>
        /// <param name="eval">The alternate caller.</param>
        /// <returns>The current evaluator.</returns>
        public Evaluator UpdateCaller(Evaluator eval)
        {
            this.CallerCaller = eval;
            return this;
        }

        /// <summary>
        /// Assign PC and return the current evaluator.
        /// </summary>
        /// <param name="nextStep">The new PC value</param>
        /// <returns>The next step to take.</returns>
        public Evaluator ContinueHere(Func<Evaluator, Evaluator> nextStep)
        {
            this.pc = nextStep;
            return this;
        }

        /// <summary>
        /// Return fram an evaluator.
        /// Assign the return value and return the caller task to resume.
        /// </summary>
        /// <param name="exp">The value to save as the returned value.</param>
        /// <param name="envir">The environment to save as the returned environment.</param>
        /// <returns>The next evaluator, which is the caller.</returns>
        public Evaluator ReturnFromStep(Obj exp, Environment envir)
        {
            return this.CallerCaller.UpdateReturnValue(exp, envir);
        }

        /// <summary>
        /// Return fram an evaluator, with the default environment
        /// </summary>
        /// <param name="exp">The value to save as the returned value.</param>
        /// <returns>The next evaluator, which is the caller.</returns>
        public Evaluator ReturnFromStep(Obj exp)
        {
            return this.CallerCaller.UpdateReturnValue(exp);
        }

        /// <summary>
        /// Returns an expression along with return flag.
        /// </summary>
        /// <param name="exp">The value to save as the returned value.</param>
        /// <param name="flag">The return type.</param>
        /// <returns>The next evaluator, which is the caller.</returns>
        public Evaluator ReturnFromStep(Obj exp, ReturnType flag)
        {
            return this.CallerCaller.UpdateReturnValue(exp, flag);
        }

        /// <summary>
        /// Return from this step and end evaluation.
        /// </summary>
        /// <returns>The ended evaluator.</returns>
        public Evaluator ReturnEnded()
        {
            return this.Interp.Ended;
        }

        /// <summary>
        /// Return the undefined result.
        /// </summary>
        /// <returns>The next evaluator, which is in the caller.</returns>
        public Evaluator ReturnUndefined()
        {
            return ReturnFromStep(Undefined.New());
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Write trace info if trace enabled.
        /// </summary>
        private void TraceStep()
        {
            if (!this.Interp.Trace)
            {
                return;
            }

            string info = this.TraceInfo();
            if (info == null)
            {
                return;
            }

            this.Interp.CurrentOutputPort.WriteLine(String.Format("{0}: {1}", info, this.Expr));
        }

        /// <summary>
        /// Dump the current evaluator into a string builder.
        /// </summary>
        /// <param name="buf">The string builder to write to.</param>#
        private void DumpStep(StringBuilder buf)
        {
            buf.AppendFormat("Exaluator {0}\n", TypePrimitives.TypeName(this));
            string exp = this.Expr.IsEmptyList() ? "()" : this.Expr.ToString();
            buf.AppendFormat("  Expr: {0}\n", exp);
            if (this.Env != null)
            {
                buf.AppendFormat("  Env:\n{0}", this.Env.Dump(1, 3));
            }
        }
        #endregion
    }
}
