// <copyright file="EvaluateLet.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate a let expression
    /// Bind the variables to values and then evaluate the expressions.
    /// </summary>
    //// <r4rs section="4.2.2">(let <bindings> <body>)</r4rs>
    //// <r4rs section="4.2.4">(let <variable> <bindings> <body>)</r4rs>
    //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
    //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
    public sealed class EvaluateLet : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        public const string StepperName = "evaluate-let";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// Name, for named let.
        /// </summary>
        private string name;

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
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateLet class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateLet(Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
            this.name = null;
            ContinueHere(InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Call let evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The let evaluator.</returns>
        public static Stepper Call(Obj expr, Environment env, Stepper caller)
        {
            return new EvaluateLet(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Start by checking the number of arguments.
        /// Then test for named let.  
        /// Grab bindings, body, and optionally name.
        /// Convert the let to a corresponding lambda.
        /// For a normal let, evaluate this lambda.
        /// For a named let, construct the additional lambda to bind to the name.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Continues by evaluating the constructed lambda.</returns>
        private static Stepper InitialStep(Stepper s)
        {
            EvaluateLet step = (EvaluateLet)s;
            if (EmptyList.IsEmptyList(s.Expr))
            {
                ErrorHandlers.SemanticError("No arguments for let");
                return s.ReturnUndefined();
            }

            if (!Pair.IsPair(s.Expr))
            {
                ErrorHandlers.SemanticError("Bad arg list for let: " + s.Expr);
                return s.ReturnUndefined();
            }

            if (Symbol.IsSymbol(List.First(s.Expr)))
            {
                // named let
                step.name = Symbol.AsSymbol(List.First(s.Expr));
                step.bindings = List.Second(s.Expr);
                step.body = List.Rest(List.Rest(s.Expr));
            }
            else
            {
                step.bindings = List.First(s.Expr);
                step.body = List.Rest(s.Expr);
            }

            if (EmptyList.IsEmptyList(step.body))
            {
                return s.ReturnUndefined();
            }

            step.vars = List.MapFun(List.First, List.New(step.bindings));
            step.inits = List.MapFun(List.Second, List.New(step.bindings));

            if (step.name == null)
            {
                // regular let -- create a closure for the body, bind inits to it, and apply it
                return EvaluateProc.Call(new Closure(step.vars, step.body, s.Env), step.inits, s.Env, s.Caller);
            }

            // named let -- eval the inits in the outer environment
            return EvaluateList.Call(step.inits, s.Env, s.ContinueHere(ApplyNamedLetStep));
        }

        /// <summary>
        /// Apply the named let.
        /// Define a closure with the bound vars and the body, and put it in the environment
        ///   of the closure.
        /// Then apply the closure.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The next step to execute.</returns>
        private static Stepper ApplyNamedLetStep(Stepper s)
        {
            EvaluateLet step = (EvaluateLet)s;
            Closure fn = new Closure(step.vars, step.body, s.Env);
            fn.Env.Define(step.name, fn);
            return fn.Apply(s.ReturnedExpr, s.Caller);
        }
        #endregion
    }
}