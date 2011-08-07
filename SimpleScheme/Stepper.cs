// <copyright file="Stepper.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Evaluates expressions step by step.
    /// </summary>
    public class Stepper
    {
        #region Constants
        /// <summary>
        /// The printable name of the scheme boolean type.
        /// </summary>
        public const string Name = "stepper";

        /// <summary>
        /// The expr in a halted stepper.
        /// </summary>
        private const string Halted = "*halted*";

        /// <summary>
        /// The expr in a suspended stepper.
        /// </summary>
        private const string Suspended = "*suspended*";
        #endregion

        #region Fields
        /// <summary>
        /// The program counter.
        /// Contains the function to execute next.
        /// </summary>
        private StepperFunction pc;

        /// <summary>
        /// Indicates whether a trace has been performed on this stepper instance.
        /// </summary>
        private bool traced;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Stepper class.
        /// This class is not instantiated itself, but only derived classes.
        /// </summary>
        /// <param name="args">The expression to evaluate.</param>
        /// <param name="env">The evaluator environment.</param>
        /// <param name="caller">The caller evaluator.</param>
        protected Stepper(Obj args, Environment env, Stepper caller)
        {
            this.Caller = caller;
            this.Expr = args;
            this.Env = env;
            this.traced = false;
        }
        #endregion

        #region Delegates
        /// <summary>
        /// This is the type for the stepper functions.
        /// It takes no arguments and returns a Stepper.
        /// These values are assigned to the pc.
        /// Steppers are meant to be static functions only.  This is because if an evaluator
        ///   instance is bound to the stepper, then it could not clone properly, so continuations
        ///   would not work.
        /// </summary>
        /// <param name="s">The stepper to invoke the step on.</param>
        /// <returns>The next step to take.</returns>
        public delegate Stepper StepperFunction(Stepper s);
        #endregion

        #region Accessors
        /// <summary>
        /// Gets a value indicating whether the stepper is halted.
        /// </summary>
        public bool IsHalted
        {
            get { return this.Expr as string == Halted; }
        }

        /// <summary>
        /// Gets a value indicating whether the stepper is suspended.
        /// </summary>
        public bool IsSuspended
        {
            get { return this.Expr as string == Suspended; }
        }

        /// <summary>
        /// Gets the interpreter.
        /// This contains the global interpretation state, such as the current ports, trace flags,
        ///   and counters.
        /// Every stepper has a copy of the interpreter, so we don't have to search down the
        ///   chain for it.
        /// This never changes, even if Env does get updated.
        /// </summary>
        public Interpreter Interp
        {
            get { return this.Env.Interp; }
        }

        /// <summary>
        /// Gets the expression being evaluated.
        /// Immutable.
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
        /// Gets the caller of this stepper.
        /// Immutable.
        /// </summary>
        public Stepper Caller { get; private set; }

        /// <summary>
        /// Gets the caller's caller.
        /// </summary>
        public Stepper CallerCaller
        {
            get { return this.Caller.Caller; }
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Tests whether to given object is a scheme stepper.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme stepper.</returns>
        public static bool Is(Obj obj)
        {
            return obj is Stepper;
        }

        /// <summary>
        /// Cast object to stepper.
        /// If obj is not really a stepper, then this is going to throw an exception.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as a stepper.</returns>
        public static Stepper As(Obj obj)
        {
            return (Stepper)obj;
        }

        /// <summary>
        /// Create a new stepper in the halted state.  This is used as the base evaluator, and contains
        ///   the given environment, which should be the global environment.
        /// </summary>
        /// <param name="env">The global environment.</param>
        /// <returns>A halted stepper.</returns>
        public static Stepper NewHalted(Environment env)
        {
            return new Stepper(Halted, env, null);
        }

        /// <summary>
        /// Create a new stepper in the suspended state.  
        /// It is used to indicate that an evaluation has suspended rather than returning a value.
        /// The IAsyncResult is given as the ReturnedExpr of the Stepper.
        /// The IAsyncResult is not that useful: it only gives info about the suspendded step, not the
        ///   final result.  There is still no way to get that without calling the async evaluator.
        /// </summary>
        /// <param name="ar">The async result that is associated with the suspension.</param>
        /// <returns>A suspended stepper.</returns>
        public static Stepper NewSuspended(IAsyncResult ar)
        {
            return new Stepper(Suspended, null, null) { ReturnedExpr = ar };
        }

        /// <summary>
        /// Transfer to a given stepper.  
        /// This can be used to return fram an evaluator.
        /// Assign the return value and return the caller task to resume.
        /// The Call/CC handler uses this to transfer to a saved continuation.
        /// </summary>
        /// <param name="nextStep">The stepper to transfer to.</param>
        /// <param name="expr">The value to save as the returned value.</param>
        /// <param name="env">The environment to save as the returned environment.</param>
        /// <returns>The next step.  This is in the caller for return.</returns>
        public static Stepper TransferToStep(Stepper nextStep, Obj expr, Environment env)
        {
            nextStep.ReturnedExpr = expr;
            nextStep.ReturnedEnv = env;
            return nextStep;
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write the stepper to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public void AsString(bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append(this.IsSuspended ? "<suspended>" : "<" + Name + ">");
            }
        }

        /// <summary>
        /// Makes a copy of the whole chain of evaluation steps, from the current step back to
        ///   the original step.  This chain is saved as the continuation.  
        /// The cloned chain is linked up with each other to form a new chain.
        /// Each subclass makes a copy of its own member variables.
        /// This is needed to support continuations, because the step contains mutable state.  If the state 
        ///  is changed after the saving of the continuation, then the continuation would not represent the
        ///  correct state.  
        /// For this shallow copy to be correct, each stepper must modify ONLY its own variables, not the
        ///  things to which they point.
        /// </summary>
        /// <returns>A copy of the current step.</returns>
        public Stepper CloneChain()
        {
            Stepper ret = (Stepper)this.MemberwiseClone();
            Stepper s = ret;
            while (s.Caller != null)
            {
                Stepper parent = (Stepper)s.Caller.MemberwiseClone();
                s.Caller = parent;
                s = s.Caller;
            }

            return ret;
        }

        /// <summary>
        /// Trace information for the step.
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
        /// Just execute the stepper function stored in pc.
        /// The instance is passed to the static function, not wrapped in the delegate.
        /// The pc is a delegate, and must not be bound to an instance, only to a static function.
        /// Otherwise, the shallow copy that is used to create a continuation
        ///   will be bound to the wrong stepper.  Check to make sure this is handled
        ///   correctly.
        /// </summary>
        /// <returns>The next step to run.</returns>
        public Stepper RunStep()
        {
            if (this.pc.Target != null)
            {
                ErrorHandlers.InternalError("Step bound to instance: " + this.pc);
            }

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
        /// <returns>A backtrace of the stepper call stack.</returns>
        public string StackBacktrace()
        {
            Stepper step = this.Caller;    // skip backtrace itself
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
        /// </summary>
        /// <param name="formals">The environment variable names.</param>
        /// <param name="vals">The values of the variables.</param>
        /// <param name="parent">The existing environment.</param>
        public void ReplaceEnvironment(Obj formals, Obj vals, Environment parent)
        {
            this.Env = new Environment(formals, vals, parent.LexicalParent);
        }

        /// <summary>
        /// Push a new environment made up of the formals and their values.
        /// Link it to the parent environment.
        /// </summary>
        /// <param name="formals">The environment variable names.</param>
        /// <param name="vals">The values of the variables.</param>
        /// <param name="parent">The lexically enclosing environment.</param>
        public void PushEnvironment(Obj formals, Obj vals, Environment parent)
        {
            this.Env = new Environment(formals, vals, parent);
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
        /// <returns>The next step, which is this stepper.</returns>
        public Stepper UpdateExpr(Obj expr)
        {
            this.Expr = expr;
            return this;
        }

        /// <summary>
        /// Continue executing in the existing evaluator, but set the returned expr.
        /// </summary>
        /// <param name="expr">The new returned expr value.</param>
        /// <returns>The next step, which is this stepper.</returns>
        public Stepper UpdateReturnedExpr(Obj expr)
        {
            this.ReturnedExpr = expr;
            return this;
        }
        
        /// <summary>
        /// Assign PC and return the current stepper.
        /// </summary>
        /// <param name="nextStep">The new PC value</param>
        /// <returns>The next step to take.</returns>
        public Stepper ContinueHere(StepperFunction nextStep)
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
        /// <returns>The next step, which is in the caller.</returns>
        public Stepper ReturnFromStep(Obj expr, Environment env)
        {
            this.Caller.ReturnedExpr = expr;
            this.Caller.ReturnedEnv = env;
            return this.Caller;
        }

        /// <summary>
        /// Return fram an evaluator, with the default environment
        /// </summary>
        /// <param name="expr">The value to save as the returned value.</param>
        /// <returns>The next step, which is in the caller.</returns>
        public Stepper ReturnFromStep(Obj expr)
        {
            this.Caller.ReturnedExpr = expr;
            return this.Caller;
        }

        /// <summary>
        /// Return the undefined result.
        /// </summary>
        /// <returns>The next step, which is in the caller.</returns>
        public Stepper ReturnUndefined()
        {
            this.Caller.ReturnedExpr = Undefined.Instance;
            return this.Caller;
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Dump the current step into a string builder.
        /// </summary>
        /// <param name="buf">The string builder to write to.</param>
        private void DumpStep(StringBuilder buf)
        {
            buf.AppendFormat("Step {0}\n", TypePrimitives.TypeName(this));
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
