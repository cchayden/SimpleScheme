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
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateLet(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
            this.name = null;
            this.Pc = this.InitialStep;
            IncrementCounter("let");
        }

        /// <summary>
        /// Call let evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The let evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
        {
            return new EvaluateLet(caller, expr, caller.Env);
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
            if (this.Expr == null)
            {
                ErrorHandlers.Error("Let: wrong number of arguments");
                return ReturnFromStep(null);
            }

            if (!(this.Expr is Pair))
            {
                ErrorHandlers.Error("Let: illegal arg list: " + this.Expr);
                return ReturnFromStep(null);
            }

            if (List.First(this.Expr) is string)
            {
                // named let
                this.name = List.First(this.Expr);
                this.bindings = List.Second(this.Expr);
                this.body = List.Rest(List.Rest(this.Expr));
            }
            else
            {
                this.bindings = List.First(this.Expr);
                this.body = List.Rest(this.Expr);
            }

            if (this.body == null)
            {
                return ReturnFromStep(null);
            }

            this.vars = List.MapFun(List.First, List.MakeList(this.bindings));
            this.inits = List.MapFun(List.Second, List.MakeList(this.bindings));

            if (this.name == null)
            {
                // regular let -- create a closure for the body, bind inits to it, and apply it
                this.Pc = this.ReturnStep;
                return EvaluateApplyProc.Call(this, this.inits, new Closure(this.vars, this.body, this.Env));
            }

            // named let -- create an outer lambda that defines the name
            object innerLambda = List.Cons("lambda", List.Cons(this.vars, this.body));
            object set = List.MakeList("set!", this.name, innerLambda);
            object invoke = List.Cons(this.name, this.inits);
            object outerLambda = List.MakeList("lambda", List.MakeList(this.name), set, invoke);
            this.Pc = this.ApplyLambda;
            return EvaluatorMain.Call(this, outerLambda);
        }

        /// <summary>
        /// Back from named let.  Execute the lambda that was defined.
        /// </summary>
        /// <returns>Execution continues with evaluation of the body of the let.</returns>
        private Stepper ApplyLambda()
        {
            // the rest of named let -- apply the lambda we defined before
            this.Pc = this.ReturnStep;
            return EvaluateApplyProc.Call(this, List.MakeList(SchemeBoolean.False), ReturnedExpr);
        }
    }
}