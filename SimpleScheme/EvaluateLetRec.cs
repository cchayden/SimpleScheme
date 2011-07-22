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
        internal const string StepperName = "evaluate-letrec";

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
            if (TypePrimitives.IsEmptyList(Expr))
            {
                ErrorHandlers.SemanticError("No arguments for letrec");
                return ReturnUndefined();
            }

            if (!TypePrimitives.IsPair(Expr))
            {
                ErrorHandlers.SemanticError("Bad arg list for letrec: " + Expr);
                return ReturnUndefined();
            }

            Obj bindings = List.First(Expr);
            this.body = List.Rest(Expr);

            if (TypePrimitives.IsEmptyList(this.body))
            {
                return ReturnUndefined();
            }

            this.vars = List.MapFun(List.First, List.New(bindings));
            this.inits = List.MapFun(List.Second, List.New(bindings));
            this.formals = this.vars;
            Obj initVals = EmptyList.Instance;
            int n = List.Length(this.vars);
            for (int i = 0; i < n; i++)
            {
                initVals = List.Cons(Undefined.Instance, initVals);
            }

            this.vals = new System.Collections.Generic.List<Obj>(n);

            this.PushEnvironment(this.formals, initVals, this.Env);
            return ContinueHere(this.EvalInit);
        }

        /// <summary>
        /// Evaluate one of the inits in the environment of the vars.
        /// </summary>
        /// <returns>The next step.</returns>
        private Stepper EvalInit()
        {
            if (TypePrimitives.IsEmptyList(this.inits))
            {
                return ContinueHere(this.ApplyProc);
            }

            Closure fun = new Closure(this.formals, List.New(List.First(this.inits)), this.Env);  
            return fun.ApplyWithtEnv(this.Env, ContinueHere(this.BindVarToInit));
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
            this.vars = List.Rest(this.vars);
            this.inits = List.Rest(this.inits);
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
            int n = List.Length(this.formals);
            for (int i = 0; i < n; i++)
            {
                this.Env.Set(List.First(var), this.vals[i]);
                var = List.Rest(var);
            }

            // apply the fun to the vals and return
            Closure fun = new Closure(this.formals, this.body, this.Env);
            return fun.ApplyWithtEnv(this.Env, this.Caller);
        }
        #endregion
    }
}