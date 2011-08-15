// <copyright file="EvaluateLetStar.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate a let* expression
    /// Bind the variables to values and then evaluate the expressions.
    /// </summary>
    //// <r4rs section="4.2.2">(let* <bindings> <body>)</r4rs>
    //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
    //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
    public sealed class EvaluateLetStar : Evaluator
    {
        #region Fields
        /// <summary>
        /// The name of the evaluator, used for counters and tracing.
        /// </summary>
        public const string EvaluatorName = "evaluate-let*";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(EvaluatorName);

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
        /// Evaluated values of inits.
        /// </summary>
        private Obj vals;

        /// <summary>
        /// The list of formal parameters to pass to the final lambda.
        /// This is built up from variable1, variable2, ...
        /// It is in the reverse order from the given list.
        /// </summary>
        private Obj formals;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateLetStar class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="body">The let* body.</param>
        /// <param name="vars">The list of variables to bind.</param>
        /// <param name="inits">The initialization expressions.</param>
        /// <param name="formals">The list of parameters to pass to the lambda.</param>
        /// <param name="vals">Evaluated values of inits.</param>
        private EvaluateLetStar(Obj expr, Environment env, Evaluator caller, Obj body, Obj vars, Obj inits, Obj formals, Obj vals)
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
        /// Call let* evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The let evaluator.</returns>
        public static Evaluator Call(Obj expr, Environment env, Evaluator caller)
        {
            if (EmptyList.Is(expr))
            {
                ErrorHandlers.SemanticError("No arguments arguments for let*");
                return caller.UpdateReturnValue(new Undefined());
            }

            if (!Pair.Is(expr))
            {
                ErrorHandlers.SemanticError("Bad arg list for let*: " + expr);
                return caller.UpdateReturnValue(new Undefined());
            }

            Obj bindings = List.First(expr);
            Obj body = List.Rest(expr);

            if (EmptyList.Is(body))
            {
                return caller.UpdateReturnValue(new Undefined());
            }

            Obj vars = List.MapFun(List.First, List.New(bindings));
            Obj inits = List.MapFun(List.Second, List.New(bindings));
            Obj formals = EmptyList.Instance;
            Obj vals = EmptyList.Instance;
            return new EvaluateLetStar(expr, env, caller, body, vars, inits, formals, vals);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Evaluate one of the inits.
        /// Do it in an environment made up of the previously evaluated inits bound to their vars.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next step.</returns>
        private static Evaluator EvalInitStep(Evaluator s)
        {
            EvaluateLetStar step = (EvaluateLetStar)s;
            if (EmptyList.Is(step.inits))
            {
                return s.ContinueHere(ApplyProcStep);
            }

            Procedure fun = new Lambda(step.formals, List.New(List.First(step.inits)), s.Env);
            return fun.Apply(step.vals, s.ContinueHere(BindVarToInitStep));
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
            EvaluateLetStar step = (EvaluateLetStar)s;
            step.formals = Pair.Cons(List.First(step.vars), step.formals);
            step.vals = Pair.Cons(s.ReturnedExpr, step.vals);
            step.vars = List.Rest(step.vars);
            step.inits = List.Rest(step.inits);
            return s.ContinueHere(EvalInitStep);
        }

        /// <summary>
        /// Inits evaluated and bound -- execute the proc.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Execution continues with evaluation of the body of the let.</returns>
        private static Evaluator ApplyProcStep(Evaluator s)
        {
            EvaluateLetStar step = (EvaluateLetStar)s;

            // apply the fun to the vals
            Lambda fun = new Lambda(step.formals, step.body, s.Env);
            return fun.Apply(step.vals, s.Caller);
        }
        #endregion
    }
}