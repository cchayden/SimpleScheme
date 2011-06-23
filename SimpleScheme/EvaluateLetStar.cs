// <copyright file="EvaluateLetStar.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a let* expression
    /// Bind the variables to values and then evaluate the expressions.
    /// </summary>
    //// <r4rs section="4.2.2">(let* <bindings> <body>)</r4rs>
    //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
    //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
    public sealed class EvaluateLetStar : Stepper
    {
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
        /// Evaluated values of inits.
        /// </summary>
        private object vals;

        /// <summary>
        /// The list of formal parameters to pass to the final closure.
        /// This is built up from variable1, variable2, ...
        /// It is in the reverse order from the given list.
        /// </summary>
        private object formals;

        /// <summary>
        /// Initializes a new instance of the EvaluateLetStar class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateLetStar(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
            this.Pc = this.InitialStep;
            IncrementCounter("let*");
        }

        /// <summary>
        /// Call let* evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The let evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
        {
            return new EvaluateLetStar(caller, expr, caller.Env);
        }

        /// <summary>
        /// Start by checking the number of arguments.
        /// Then test for named let.  
        /// Grab bindings, body, vars, and inits.
        /// </summary>
        /// <returns>Continues by evaluating the init list.</returns>
        private Stepper InitialStep()
        {
            if (this.Expr == null)
            {
                ErrorHandlers.Error("Let*: wrong number of arguments");
                return ReturnFromStep(null);
            }

            if (!(this.Expr is Pair))
            {
                ErrorHandlers.Error("Let*: illegal arg list: " + this.Expr);
                return ReturnFromStep(null);
            }

            this.bindings = List.First(this.Expr);
            this.body = List.Rest(this.Expr);

            if (this.body == null)
            {
                return ReturnFromStep(null);
            }

            this.vars = List.MapFun(List.First, List.MakeList(this.bindings));
            this.inits = List.MapFun(List.Second, List.MakeList(this.bindings));
            this.formals = null;
            this.vals = null;
            this.Pc = this.EvalInit;
            return this;
        }

        /// <summary>
        /// Evaluate one of the inits.
        /// Do it in an environment made up of the previously evaluated inits bound to their vars.
        /// </summary>
        /// <returns>The next step.</returns>
        private Stepper EvalInit()
        {
            if (this.inits == null)
            {
                this.Pc = this.ApplyLambda1;
                return this;
            }

            object fun = new Closure(this.formals, List.MakeList(List.First(this.inits)), this.Env);
            this.Pc = this.BindVarToInit;
            return Procedure.Proc(fun).Apply(this, this.vals);
        }

        /// <summary>
        /// Add var to list of formals, evaluation result to list of vals.
        /// Step down list of vars and inits.
        /// Go back and evaluate another init.
        /// </summary>
        /// <returns>The next step.</returns>
        private Stepper BindVarToInit()
        {
            this.formals = List.Cons(List.First(this.vars), this.formals);
            this.vals = List.Cons(ReturnedExpr, this.vals);
            this.vars = List.Rest(this.vars);
            this.inits = List.Rest(this.inits);
            this.Pc = this.EvalInit;
            return this;
        }

        /// <summary>
        /// Inits evaluated and bound -- execute the proc.
        /// </summary>
        /// <returns>Execution continues with evaluation of the body of the let.</returns>
        private Stepper ApplyLambda1()
        {
            // apply the fun to the vals
            this.Pc = this.ReturnStep;
            object fun = new Closure(this.formals, this.body, this.Env);
            return Procedure.Proc(fun).Apply(this, this.vals);
        }
    }
}