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
    internal sealed class EvaluateLetStar : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        internal const string StepperName = "evaluate-let*";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// The variable bindings established by the let expression.
        /// </summary>
        private Obj bindings;

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
        /// Evaluated values of inits.
        /// </summary>
        private Obj vals;

        /// <summary>
        /// The list of formal parameters to pass to the final closure.
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
        private EvaluateLetStar(Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
            ContinueHere(InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Call let* evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The let evaluator.</returns>
        internal static Stepper Call(Obj expr, Environment env, Stepper caller)
        {
            return new EvaluateLetStar(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Start by checking the number of arguments.
        /// Then test for named let.  
        /// Grab bindings, body, vars, and inits.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Continues by evaluating the init list.</returns>
        private static Stepper InitialStep(Stepper s)
        {
            EvaluateLetStar step = (EvaluateLetStar)s;
            if (EmptyList.IsEmptyList(s.Expr))
            {
                ErrorHandlers.SemanticError("No arguments arguments for let*");
                return s.ReturnUndefined();
            }

            if (!Pair.IsPair(s.Expr))
            {
                ErrorHandlers.SemanticError("Bad arg list for let*: " + s.Expr);
                return s.ReturnUndefined();
            }

            step.bindings = List.First(s.Expr);
            step.body = List.Rest(s.Expr);

            if (EmptyList.IsEmptyList(step.body))
            {
                return s.ReturnUndefined();
            }

            step.vars = List.MapFun(List.First, List.New(step.bindings));
            step.inits = List.MapFun(List.Second, List.New(step.bindings));
            step.formals = EmptyList.Instance;
            step.vals = EmptyList.Instance;
            return s.ContinueHere(EvalInitStep);
        }

        /// <summary>
        /// Evaluate one of the inits.
        /// Do it in an environment made up of the previously evaluated inits bound to their vars.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The next step.</returns>
        private static Stepper EvalInitStep(Stepper s)
        {
            EvaluateLetStar step = (EvaluateLetStar)s;
            if (EmptyList.IsEmptyList(step.inits))
            {
                return s.ContinueHere(ApplyLambdaStep);
            }

            Procedure fun = new Closure(step.formals, List.New(List.First(step.inits)), s.Env);
            return fun.Apply(step.vals, s.ContinueHere(BindVarToInitStep));
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
            EvaluateLetStar step = (EvaluateLetStar)s;
            step.formals = List.Cons(List.First(step.vars), step.formals);
            step.vals = List.Cons(s.ReturnedExpr, step.vals);
            step.vars = List.Rest(step.vars);
            step.inits = List.Rest(step.inits);
            return s.ContinueHere(EvalInitStep);
        }

        /// <summary>
        /// Inits evaluated and bound -- execute the proc.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Execution continues with evaluation of the body of the let.</returns>
        private static Stepper ApplyLambdaStep(Stepper s)
        {
            EvaluateLetStar step = (EvaluateLetStar)s;

            // apply the fun to the vals
            Closure fun = new Closure(step.formals, step.body, s.Env);
            return fun.Apply(step.vals, s.Caller);
        }
        #endregion
    }
}