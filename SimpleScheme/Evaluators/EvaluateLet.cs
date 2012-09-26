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
    public sealed class EvaluateLet : Evaluator
    {
        #region Fields
        /// <summary>
        /// The name of the evaluator, used for counters and tracing.
        /// </summary>
        public const string EvaluatorName = "evaluate-let";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(EvaluatorName);

        /// <summary>
        /// Name, for named let.
        /// </summary>
        private readonly Symbol name;

        /// <summary>
        /// The body of the let.
        /// </summary>
        private readonly Obj body;

        /// <summary>
        /// The list of variables to bind.
        /// </summary>
        private readonly Obj vars;

        /// <summary>
        /// This list of initial expressions.
        /// </summary>
        private readonly Obj inits;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateLet class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="name">The name of a named let.</param>
        /// <param name="body">The let body.</param>
        /// <param name="vars">The variables to bind.</param>
        /// <param name="inits">The initial values of the variables.</param>
        private EvaluateLet(Obj expr, Environment env, Evaluator caller, Symbol name, Obj body, Obj vars, Obj inits)
            : base(expr, env, caller)
        {
            this.name = name;
            this.body = body;
            this.vars = vars;
            this.inits = inits;
            ContinueHere(InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Call let evaluator.
        /// Start by checking the number of arguments.
        /// Then test for named let.  
        /// Grab bindings, body, and optionally name.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The let evaluator.</returns>
        public static Evaluator Call(Obj expr, Environment env, Evaluator caller)
        {
            if (expr.IsEmptyList())
            {
                ErrorHandlers.SemanticError("No arguments for let");
                return caller.UpdateReturnValue(Undefined.New());
            }

            if (!expr.IsPair())
            {
                ErrorHandlers.SemanticError("Bad arg list for let: " + expr);
                return caller.UpdateReturnValue(Undefined.New());
            }

            Symbol name = null;
            Obj bindings;
            Obj body;
            if (expr.First().IsSymbol())
            {
                // named let
                name = expr.First().AsSymbol();   
                bindings = expr.Second();
                body = expr.Rest().Rest();
            }
            else
            {
                bindings = expr.First();
                body = expr.Rest();
            }

            if (body.IsEmptyList())
            {
                return caller.UpdateReturnValue(Undefined.New());
            }

            Obj vars = List.MapFun(List.First, bindings.MakeList());
            Obj inits = List.MapFun(List.Second, bindings.MakeList());
            return new EvaluateLet(expr, env, caller, name, body, vars, inits);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Convert the let to a corresponding lambda.
        /// For a normal let, evaluate this lambda.
        /// For a named let, construct the additional lambda to bind to the name.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Continues by evaluating the constructed lambda.</returns>
        private static Evaluator InitialStep(Evaluator s)
        {
            var step = (EvaluateLet)s;
            if (step.name == null)
            {
                // regular let -- create a lambda for the body, bind inits to it, and apply it
                return EvaluateProc.Call(Lambda.New(step.vars, step.body, s.Env), step.inits, s.Env, s.Caller);
            }

            // named let -- eval the inits in the outer environment
            return EvaluateList.Call(step.inits, s.Env, s.ContinueHere(ApplyNamedLetStep));
        }

        /// <summary>
        /// Apply the named let.
        /// Define a lambda with the bound vars and the body, and put it in the environment
        ///   of the lambda.
        /// Then apply the lambda.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next evaluator to execute.</returns>
        private static Evaluator ApplyNamedLetStep(Evaluator s)
        {
            var step = (EvaluateLet)s;
            Lambda fn = Lambda.New(step.vars, step.body, s.Env);
            fn.Env.Define(step.name, fn);   
            return fn.Apply(s.ReturnedExpr, s.Caller);
        }
        #endregion
    }
}