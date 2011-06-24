// <copyright file="EvaluateLetRec.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate a letrec expression
    /// Bind the variables to values and then evaluate the expressions.
    /// </summary>
    //// <r4rs section="4.2.2">(letrec<bindings> <body>)</r4rs>
    //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
    //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
    internal sealed class EvaluateLetRec : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "evaluate-letrec";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// The body of the let.
        /// </summary>
        private Obj body;

        /// <summary>
        /// The list of variables to bind.
        /// </summary>
        private Obj vars;

        /// <summary>
        /// This list of initial expressions.
        /// </summary>
        private Obj inits;

        /// <summary>
        /// The list of formal parameters to pass to the final closure.
        /// This is variable1, variable2, ...
        /// </summary>
        private Obj formals;

        /// <summary>
        /// Accumulate the list of init vals here for later assignment.
        /// </summary>
        private System.Collections.Generic.List<Obj> vals;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateLetRec class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateLetRec(Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
            ContinueHere(this.InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the name of the stepper.
        /// </summary>
        internal override string Name
        {
            get { return StepperName; }
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Call letrec evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The let evaluator.</returns>
        internal static Stepper Call(Obj expr, Environment env, Stepper caller)
        {
            return new EvaluateLetRec(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Start by checking the number of arguments.
        /// Grab bindings, body, vars, and inits.
        /// Make an environment for evaluating the inits consisting of the vars and 
        ///   an equal number of undefined vals.
        /// </summary>
        /// <returns>Continues by evaluating the init list.</returns>
        private Stepper InitialStep()
        {
            if (Expr == List.Empty)
            {
                ErrorHandlers.Error("Letrec: wrong number of arguments");
                return ReturnUndefined();
            }

            if (!(Expr is Pair))
            {
                ErrorHandlers.Error("Letrec: illegal arg list: " + Expr);
                return ReturnUndefined();
            }

            Obj bindings = First(Expr);
            this.body = Rest(Expr);

            if (this.body == List.Empty)
            {
                return ReturnUndefined();
            }

            this.vars = MapFun(First, MakeList(bindings));
            this.inits = MapFun(Second, MakeList(bindings));
            this.formals = this.vars;
            Obj initVals = List.Empty;
            int n = Length(this.vars);
            for (int i = 0; i < n; i++)
            {
                initVals = Cons(Undefined.Instance, initVals);
            }

            this.vals = new System.Collections.Generic.List<Obj>(n);

            this.ReplaceEnvironment(this.formals, initVals, this.CallerEnv);
            return ContinueHere(this.EvalInit);
        }

        /// <summary>
        /// Evaluate one of the inits in the environment of the vars.
        /// </summary>
        /// <returns>The next step.</returns>
        private Stepper EvalInit()
        {
            if (this.inits == List.Empty)
            {
                return ContinueHere(this.ApplyProc);
            }

            Closure fun = Closure.New(this.formals, MakeList(First(this.inits)), this.Env);  
            return fun.ApplyWithCurrentEnv(ContinueHere(this.BindVarToInit));
        }

        /// <summary>
        /// Add var to list of formals, evaluation result to list of vals.
        /// Step down list of vars and inits.
        /// Go back and evaluate another init.
        /// </summary>
        /// <returns>The next step.</returns>
        private Stepper BindVarToInit()
        {
            this.vals.Add(ReturnedExpr);
            this.vars = Rest(this.vars);
            this.inits = Rest(this.inits);
            return ContinueHere(this.EvalInit);
        }

        /// <summary>
        /// Inits evaluated and bound -- execute the proc and return.
        /// </summary>
        /// <returns>Execution returns to the caller.</returns>
        private Stepper ApplyProc()
        {
            // assign the inits into the env
            Obj var = this.formals;
            int n = Length(this.formals);
            for (int i = 0; i < n; i++)
            {
                this.Env.Set(First(var), this.vals[i]);
                var = Rest(var);
            }

            // apply the fun to the vals and return
            Closure fun = Closure.New(this.formals, this.body, this.Env);
            return fun.ApplyWithCurrentEnv(ContinueReturn());
        }
        #endregion
    }
}