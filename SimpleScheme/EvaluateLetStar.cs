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
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "let*";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

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
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateLetStar(Stepper caller, object expr, Environment env)
            : base(caller, expr, env)
        {
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
            if (Expr == null)
            {
                ErrorHandlers.Error("Let*: wrong number of arguments");
                return ReturnFromStep(null);
            }

            if (!(Expr is Pair))
            {
                ErrorHandlers.Error("Let*: illegal arg list: " + Expr);
                return ReturnFromStep(null);
            }

            this.bindings = First(Expr);
            this.body = Rest(Expr);

            if (this.body == null)
            {
                return ReturnFromStep(null);
            }

            this.vars = MapFun(First, MakeList(this.bindings));
            this.inits = MapFun(Second, MakeList(this.bindings));
            this.formals = null;
            this.vals = null;
            return ContinueHere(this.EvalInit);
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
                return ContinueHere(this.ApplyLambda);
            }

            object fun = new Closure(this.formals, MakeList(First(this.inits)), this.Env);
            return Procedure.Proc(fun).Apply(ContinueHere(this.BindVarToInit), this.vals);
        }

        /// <summary>
        /// Add var to list of formals, evaluation result to list of vals.
        /// Step down list of vars and inits.
        /// Go back and evaluate another init.
        /// </summary>
        /// <returns>The next step.</returns>
        private Stepper BindVarToInit()
        {
            this.formals = Cons(First(this.vars), this.formals);
            this.vals = Cons(ReturnedExpr, this.vals);
            this.vars = Rest(this.vars);
            this.inits = Rest(this.inits);
            return ContinueHere(this.EvalInit);
        }

        /// <summary>
        /// Inits evaluated and bound -- execute the proc.
        /// </summary>
        /// <returns>Execution continues with evaluation of the body of the let.</returns>
        private Stepper ApplyLambda()
        {
            // apply the fun to the vals
            object fun = new Closure(this.formals, this.body, this.Env);
            return Procedure.Proc(fun).Apply(ContinueReturn(), this.vals);
        }
    }
}