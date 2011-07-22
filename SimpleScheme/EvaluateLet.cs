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
    internal sealed class EvaluateLet : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        internal const string StepperName = "evaluate-let";

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
            ContinueHere(this.InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Call let evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The let evaluator.</returns>
        internal static Stepper Call(Obj expr, Environment env, Stepper caller)
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
        /// <returns>Continues by evaluating the constructed lambda.</returns>
        private Stepper InitialStep()
        {
            if (TypePrimitives.IsEmptyList(Expr))
            {
                ErrorHandlers.SemanticError("No arguments for let");
                return ReturnUndefined();
            }

            if (!TypePrimitives.IsPair(Expr))
            {
                ErrorHandlers.SemanticError("Bad arg list for let: " + Expr);
                return ReturnUndefined();
            }

            if (TypePrimitives.IsSymbol(List.First(Expr)))
            {
                // named let
                this.name = Symbol.Sym(List.First(Expr));
                this.bindings = List.Second(Expr);
                this.body = List.Rest(List.Rest(Expr));
            }
            else
            {
                this.bindings = List.First(Expr);
                this.body = List.Rest(Expr);
            }

            if (TypePrimitives.IsEmptyList(this.body))
            {
                return ReturnUndefined();
            }

            this.vars = List.MapFun(List.First, List.New(this.bindings));
            this.inits = List.MapFun(List.Second, List.New(this.bindings));

            if (this.name == null)
            {
                // regular let -- create a closure for the body, bind inits to it, and apply it
                return EvaluateProc.Call(new Closure(this.vars, this.body, this.Env), this.inits, this.Env, this.Caller);
            }

            // named let -- eval the inits in the outer environment
            return EvaluateList.Call(this.inits, this.Env, ContinueHere(this.ApplyNamedLet));
        }

        /// <summary>
        /// Apply the named let.
        /// Define a closure with the bound vars and the body, and put it in the environment
        ///   of the closure.
        /// Then apply the closure.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        private Stepper ApplyNamedLet()
        {
            Closure fn = new Closure(this.vars, this.body, this.Env);
            fn.Env.Define(this.name, fn);
            return fn.Apply(ReturnedExpr, this.Caller);
        }
        #endregion
    }
}