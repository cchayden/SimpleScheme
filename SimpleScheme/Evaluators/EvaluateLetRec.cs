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
    public sealed class EvaluateLetRec : Evaluator
    {
        #region Fields
        /// <summary>
        /// The name of the evaluator, used for counters and tracing.
        /// </summary>
        public const string EvaluatorName = "evaluate-letrec";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(EvaluatorName);

        /// <summary>
        /// The body of the let.
        /// </summary>
        private readonly Obj body;

        /// <summary>
        /// The list of formal parameters to pass to the final lambda.
        /// This is variable1, variable2, ...
        /// </summary>
        private readonly Obj formals;

        /// <summary>
        /// The list of variables to bind.
        /// </summary>
        private Obj vars;

        /// <summary>
        /// This list of initial expressions.
        /// </summary>
        private Obj inits;

        /// <summary>
        /// Accumulate the list of init vals here for later assignment.
        /// </summary>
        private Obj vals;
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
        private EvaluateLetRec(Obj expr, Environment env, Evaluator caller, Obj body, Obj vars, Obj inits)
            : base(expr, env, caller)
        {
            this.body = body;
            this.vars = vars;
            this.inits = inits;
            this.formals = vars;
            this.vals = EmptyList.New();
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
        public static Evaluator Call(Obj expr, Environment env, Evaluator caller)
        {
            if (expr.IsEmptyList())
            {
                ErrorHandlers.SemanticError("No arguments for letrec");
                return caller.UpdateReturnValue(Undefined.New());
            }

            if (!expr.IsPair())
            {
                ErrorHandlers.SemanticError("Bad arg list for letrec: " + expr);
                return caller.UpdateReturnValue(Undefined.New());
            }

            Obj body = expr.Rest();
            if (body.IsEmptyList())
            {
                return caller.UpdateReturnValue(Undefined.New());
            }

            Obj bindings = expr.First();
            Obj formals = List.MapFun(List.First, bindings.MakeList());
            Obj inits = List.MapFun(List.Second, bindings.MakeList());
            Obj initVals = List.Fill(formals.ListLength(), Undefined.New());

            return new EvaluateLetRec(expr, new Environment(formals, initVals, env), caller, body, formals, inits);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Evaluate one of the inits in the environment of the vars.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next step.</returns>
        private static Evaluator EvalInitStep(Evaluator s)
        {
            var step = (EvaluateLetRec)s;
            if (step.inits.IsEmptyList())
            {
                return s.ContinueHere(ApplyProcStep);
            }

            Lambda fun = Lambda.New(step.formals, step.inits.First().MakeList(), s.Env);  
            return fun.ApplyWithtEnv(s.Env, s.ContinueHere(BindVarToInitStep));
        }

        /// <summary>
        /// Add var to list of formals, evaluation result to list of vals.
        /// Move down list of vars and inits.
        /// Go back and evaluate another init.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next step.</returns>
        private static Evaluator BindVarToInitStep(Evaluator s)
        {
            var step = (EvaluateLetRec)s;
            step.vals = s.ReturnedExpr.Cons(step.vals);
            step.vars = step.vars.Rest();
            step.inits = step.inits.Rest();
            return s.ContinueHere(EvalInitStep);
        }

        /// <summary>
        /// Inits evaluated and bound -- execute the proc and return.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Execution returns to the caller.</returns>
        private static Evaluator ApplyProcStep(Evaluator s)
        {
            var step = (EvaluateLetRec)s;

            // assign the inits into the env
            Obj var = step.formals;
            Obj vals = step.vals.ReverseList();
            int n = step.formals.ListLength();
            for (int i = 0; i < n; i++)
            {
                s.Env.Set(var.First(), vals.First());
                var = var.Rest();
                vals = vals.Rest();
            }

            // apply the fun to the vals and return
            Lambda fun = Lambda.New(step.formals, step.body, s.Env);
            return fun.ApplyWithtEnv(s.Env, s.Caller);
        }
        #endregion
    }
}