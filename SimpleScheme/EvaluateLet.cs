// <copyright file="EvaluateLet.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
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
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "evaluate-let";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// Name, for named let.
        /// </summary>
        private object name;

        /// <summary>
        /// The variable bindings established by the let expression.
        /// </summary>
        private object bindings;

        /// <summary>
        /// The body of the let.
        /// </summary>
        private object body;

        /// <summary>
        /// The list of variables to bind.
        /// </summary>
        private object vars;

        /// <summary>
        /// This list of initial expressions.
        /// </summary>
        private object inits;

        /// <summary>
        /// Initializes a new instance of the EvaluateLet class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateLet(object expr, Environment env, Stepper caller)
            : base(caller, expr, env)
        {
            this.name = Undefined.Instance;
            ContinueHere(this.InitialStep);
            IncrementCounter(counter);
        }

        /// <summary>
        /// Gets the name of the stepper.
        /// </summary>
        public override string Name
        {
            get { return StepperName; }
        }

        /// <summary>
        /// Call let evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The let evaluator.</returns>
        public static Stepper Call(object expr, Stepper caller)
        {
            return new EvaluateLet(expr, caller.Env, caller);
        }

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
            if (Expr == List.Empty)
            {
                ErrorHandlers.Error("Let: wrong number of arguments");
                return ReturnFromStep(Undefined.Instance);
            }

            if (!(Expr is Pair))
            {
                ErrorHandlers.Error("Let: illegal arg list: " + Expr);
                return ReturnFromStep(Undefined.Instance);
            }

            if (First(Expr) is string)
            {
                // named let
                this.name = First(Expr);
                this.bindings = Second(Expr);
                this.body = Rest(Rest(Expr));
            }
            else
            {
                this.bindings = First(Expr);
                this.body = Rest(Expr);
            }

            if (this.body == List.Empty)
            {
                return ReturnFromStep(Undefined.Instance);
            }

            this.vars = MapFun(First, MakeList(this.bindings));
            this.inits = MapFun(Second, MakeList(this.bindings));

            if (this.name == Undefined.Instance)
            {
                // regular let -- create a closure for the body, bind inits to it, and apply it
                return EvaluateProc.Call(new Closure(this.vars, this.body, this.Env), this.inits, ContinueReturn());
            }

            // named let -- eval the inits in the outer environment
            return EvaluateList.Call(ContinueHere(this.ApplyNamedLet), this.inits);
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
            return fn.Apply(ReturnedExpr, ContinueReturn());
        }
    }
}