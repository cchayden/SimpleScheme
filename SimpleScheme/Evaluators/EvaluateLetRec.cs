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
    public sealed class EvaluateLetRec : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        public const string StepperName = "evaluate-letrec";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// The body of the let.
        /// </summary>
        private readonly Obj body;

        /// <summary>
        /// The list of variables to bind.
        /// </summary>
        private Obj vars;

        /// <summary>
        /// This list of initial expressions.
        /// </summary>
        private Obj inits;

        /// <summary>
        /// The list of formal parameters to pass to the final lambda.
        /// This is variable1, variable2, ...
        /// </summary>
        private readonly Obj formals;

        /// <summary>
        /// Accumulate the list of init vals here for later assignment.
        /// </summary>
        private readonly System.Collections.Generic.List<Obj> vals;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateLetRec class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="body">The let body.</param>
        /// <param name="vars">The variables to be bound.</param>
        /// <param name="inits">The initialization expressions.</param>
        /// <param name="formals">The formal parameters.</param>
        /// <param name="vals">The initial values.</param>
        private EvaluateLetRec(Obj expr, Environment env, Stepper caller, Obj body, Obj vars, Obj inits, Obj formals, System.Collections.Generic.List<Obj> vals)
            : base(expr, env, caller)
        {
            this.body = body;
            this.vars = vars;
            this.inits = inits;
            this.formals = formals;
            this.vals = vals;
            ContinueHere(EvalInitStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Call letrec evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The let evaluator.</returns>
        public static Stepper Call(Obj expr, Environment env, Stepper caller)
        {
            if (EmptyList.Is(expr))
            {
                ErrorHandlers.SemanticError("No arguments for letrec");
                return caller.UpdateReturnedExpr(Undefined.Instance);
            }

            if (!Pair.Is(expr))
            {
                ErrorHandlers.SemanticError("Bad arg list for letrec: " + expr);
                return caller.UpdateReturnedExpr(Undefined.Instance);
            }

            Obj bindings = List.First(expr);
            Obj body = List.Rest(expr);

            if (EmptyList.Is(body))
            {
                return caller.UpdateReturnedExpr(Undefined.Instance);
            }

            Obj vars = List.MapFun(List.First, List.New(bindings));
            Obj inits = List.MapFun(List.Second, List.New(bindings));
            Obj formals = vars;
            Obj initVals = EmptyList.Instance;
            int n = List.Length(vars);
            for (int i = 0; i < n; i++)
            {
                initVals = Pair.Cons(Undefined.Instance, initVals);
            }

            System.Collections.Generic.List<Obj> vals = new System.Collections.Generic.List<Obj>(n);

            EvaluateLetRec eval = new EvaluateLetRec(expr, env, caller, body, vars, inits, formals, vals);
            eval.PushEnvironment(formals, initVals, env);
            return eval;
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Evaluate one of the inits in the environment of the vars.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The next step.</returns>
        private static Stepper EvalInitStep(Stepper s)
        {
            EvaluateLetRec step = (EvaluateLetRec)s;
            if (EmptyList.Is(step.inits))
            {
                return s.ContinueHere(ApplyProcStep);
            }

            Lambda fun = new Lambda(step.formals, List.New(List.First(step.inits)), s.Env);  
            return fun.ApplyWithtEnv(s.Env, s.ContinueHere(BindVarToInitStep));
        }

        /// <summary>
        /// Add var to list of formals, evaluation result to list of vals.
        /// Step down list of vars and inits.
        /// Go back and evaluate another init.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The next step.</returns>
        private static Stepper BindVarToInitStep(Stepper s)
        {
            EvaluateLetRec step = (EvaluateLetRec)s;
            step.vals.Add(s.ReturnedExpr);
            step.vars = List.Rest(step.vars);
            step.inits = List.Rest(step.inits);
            return s.ContinueHere(EvalInitStep);
        }

        /// <summary>
        /// Inits evaluated and bound -- execute the proc and return.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Execution returns to the caller.</returns>
        private static Stepper ApplyProcStep(Stepper s)
        {
            EvaluateLetRec step = (EvaluateLetRec)s;

            // assign the inits into the env
            Obj var = step.formals;
            int n = List.Length(step.formals);
            for (int i = 0; i < n; i++)
            {
                s.Env.Set(List.First(var), step.vals[i]);
                var = List.Rest(var);
            }

            // apply the fun to the vals and return
            Lambda fun = new Lambda(step.formals, step.body, s.Env);
            return fun.ApplyWithtEnv(s.Env, s.Caller);
        }
        #endregion
    }
}