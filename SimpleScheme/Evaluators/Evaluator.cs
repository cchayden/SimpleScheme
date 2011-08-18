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
        /// Evaluators are meant to be static functions only.  This is because if an evaluator
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
            this.Caller = caller;
            this.Expr = args;
            this.Env = env;
            this.ReturnedExpr = new Undefined();
            this.caught = 0;
            this.traced = false;
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
        public Obj Expr { get; private set; }

        /// <summary>
        /// Gets the returned expression from the last call.
        /// </summary>
        public Obj ReturnedExpr { get; private set; }

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
        public Evaluator Caller { get; private set; }

        /// <summary>
        /// Gets the caller's caller.
        /// </summary>
        public Evaluator CallerCaller
        {
            get { return this.Caller.Caller; }
        }

        /// <summary>
        /// Gets tne number of caught evaluations
        /// </summary>
        public int Caught
        {
            get { return this.caught; }
        }

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
        /// Tests whether to given object is a scheme evaluator.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme evaluator.</returns>
        public static bool Is(Obj obj)
        {
            return obj is Evaluator;
        }

        /// <summary>
        /// Cast object to an evaluator.
        /// If obj is not really a evaluator, then this is going to throw an exception.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as an evaluator.</returns>
        public static Evaluator As(Obj obj)
        {
            return (Evaluator)obj;
        }

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
        public override void AsString(bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append("<" + Name + ">");
            }
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
            Evaluator ret = (Evaluator)this.MemberwiseClone();
            Evaluator s = ret;
            while (s.Caller != null)
            {
                Evaluator parent = (Evaluator)s.Caller.MemberwiseClone();
                s.Caller = parent;
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
        ///   will be bound to the wrong evaluator.  Check to make sure this is handled
        ///   correctly.
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
        /// <param name="expr">The new expr value.</param>
        /// <returns>The next evaluator, which is this evaluator.</returns>
        public Evaluator UpdateExpr(Obj expr)
        {
            this.Expr = expr;
            return this;
        }

        /// <summary>
        /// Continue executing in the existing evaluator, but step
        ///   one element down the list.
        /// </summary>
        /// <returns>The next evaluator, which is this evaluator.</returns>
        public Evaluator StepDownExpr()
        {
            this.Expr = List.Rest(this.Expr);
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
        /// Decrement the caught counter.
        /// </summary>
        /// <returns>The new value of the caught flag.</returns>
        public int DecrementCaught()
        {
            Interlocked.Decrement(ref this.caught);
            return this.caught;
        }

        /// <summary>
        /// DecrementReset the caught counter.
        /// </summary>
        /// <returns>The new value of the caught flag.</returns>
        public int ResetCaught()
        {
            this.caught = 0;
            return this.caught;
        }

        /// <summary>
        /// Continue executing in this evaluator, but set the returned expr.
        /// Usually invoked on an object's caller.  
        /// </summary>
        /// <param name="expr">The returned value.</param>
        /// <returns>The next evaluator, which is this evaluator.</returns>
        public Evaluator UpdateReturnValue(Obj expr)
        {
            this.ReturnedExpr = expr;
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
            this.Caller = eval;
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
        /// <param name="expr">The value to save as the returned value.</param>
        /// <param name="env">The environment to save as the returned environment.</param>
        /// <returns>The next evaluator, which is in the caller.</returns>
        public Evaluator ReturnFromStep(Obj expr, Environment env)
        {
            this.Caller.ReturnedExpr = expr;
            this.Caller.ReturnedEnv = env;
            return this.Caller;
        }

        /// <summary>
        /// Return fram an evaluator, with the default environment
        /// </summary>
        /// <param name="expr">The value to save as the returned value.</param>
        /// <returns>The next evaluator, which is in the caller.</returns>
        public Evaluator ReturnFromStep(Obj expr)
        {
            this.Caller.ReturnedExpr = expr;
            return this.Caller;
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
            this.Caller.ReturnedExpr = new Undefined();
            return this.Caller;
        }

        /// <summary>
        /// Returns undefined result.
        /// Also sets the interal value field.
        /// </summary>
        /// <param name="value">The value to set.</param>
        /// <returns>The next evaluator, which is in the caller.</returns>
        public Evaluator ReturnUndefined(int value)
        {
            this.Caller.ReturnedExpr = new Undefined(value);
            return this.Caller;
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
            string exp = EmptyList.Is(this.Expr) ? "()" : this.Expr.ToString();
            buf.AppendFormat("  Expr: {0}\n", exp);
            if (this.Env != null)
            {
                buf.AppendFormat("  Env:\n{0}", this.Env.Dump(1, 3));
            }
        }
        #endregion
    }
}
