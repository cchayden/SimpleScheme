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
            ContinueHere(InitialStep);
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
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Continues by evaluating the init list.</returns>
        private static Stepper InitialStep(Stepper s)
        {
            EvaluateLetRec step = (EvaluateLetRec)s;
            if (EmptyList.IsEmptyList(s.Expr))
            {
                ErrorHandlers.SemanticError("No arguments for letrec");
                return s.ReturnUndefined();
            }

            if (!Pair.IsPair(s.Expr))
            {
                ErrorHandlers.SemanticError("Bad arg list for letrec: " + s.Expr);
                return s.ReturnUndefined();
            }

            Obj bindings = List.First(s.Expr);
            step.body = List.Rest(s.Expr);

            if (EmptyList.IsEmptyList(step.body))
            {
                return s.ReturnUndefined();
            }

            step.vars = List.MapFun(List.First, List.New(bindings));
            step.inits = List.MapFun(List.Second, List.New(bindings));
            step.formals = step.vars;
            Obj initVals = EmptyList.Instance;
            int n = List.Length(step.vars);
            for (int i = 0; i < n; i++)
            {
                initVals = List.Cons(Undefined.Instance, initVals);
            }

            step.vals = new System.Collections.Generic.List<Obj>(n);

            s.PushEnvironment(step.formals, initVals, s.Env);
            return s.ContinueHere(EvalInitStep);
        }

        /// <summary>
        /// Evaluate one of the inits in the environment of the vars.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The next step.</returns>
        private static Stepper EvalInitStep(Stepper s)
        {
            EvaluateLetRec step = (EvaluateLetRec)s;
            if (EmptyList.IsEmptyList(step.inits))
            {
                return s.ContinueHere(ApplyProcStep);
            }

            Closure fun = new Closure(step.formals, List.New(List.First(step.inits)), s.Env);  
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
            Closure fun = new Closure(step.formals, step.body, s.Env);
            return fun.ApplyWithtEnv(s.Env, s.Caller);
        }
        #endregion
    }
}