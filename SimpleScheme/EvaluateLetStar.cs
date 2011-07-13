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
        private const string StepperName = "evaluate-let*";

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
            ContinueHere(this.InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the name of the stepper.
        /// </summary>
        internal override string Name
        {
            get { return StepperName; }
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
        /// <returns>Continues by evaluating the init list.</returns>
        private Stepper InitialStep()
        {
            if (EmptyList.IsType(Expr))
            {
                ErrorHandlers.SemanticError("No arguments arguments for let*");
                return ReturnUndefined();
            }

            if (!(Expr is Pair))
            {
                ErrorHandlers.SemanticError("Bad arg list for let*: " + Expr);
                return ReturnUndefined();
            }

            this.bindings = First(Expr);
            this.body = Rest(Expr);

            if (EmptyList.IsType(this.body))
            {
                return ReturnUndefined();
            }

            this.vars = MapFun(First, MakeList(this.bindings));
            this.inits = MapFun(Second, MakeList(this.bindings));
            this.formals = List.Empty;
            this.vals = List.Empty;
            return ContinueHere(this.EvalInit);
        }

        /// <summary>
        /// Evaluate one of the inits.
        /// Do it in an environment made up of the previously evaluated inits bound to their vars.
        /// </summary>
        /// <returns>The next step.</returns>
        private Stepper EvalInit()
        {
            if (EmptyList.IsType(this.inits))
            {
                return ContinueHere(this.ApplyLambda);
            }

            Procedure fun = Closure.New(this.formals, MakeList(First(this.inits)), this.Env);
            return fun.Apply(this.vals, this.Env, ContinueHere(this.BindVarToInit));
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
            Procedure fun = Closure.New(this.formals, this.body, this.Env);
            return fun.Apply(this.vals, this.Caller.Env, this.Caller);
        }
        #endregion
    }
}