// <copyright file="EvaluateLetStar.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Evaluate a let* expression
    /// Bind the variables to values and then evaluate the expressions.
    /// </summary>
    //// <r4rs section="4.2.2">(let* <bindings> <body>)</r4rs>
    //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
    //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
    internal sealed class EvaluateLetStar : Evaluator
    {
        #region Fields
        /// <summary>
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper evalInitStep = GetStepper("EvalInitStep");

        /// <summary>
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper bindVarToInitStep = GetStepper("BindVarToInitStep");

        /// <summary>
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper applyProcStep = GetStepper("ApplyProcStep");

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-let*");

        /// <summary>
        /// The body of the let.
        /// </summary>
        private readonly SchemeObject body;

        /// <summary>
        /// The list of variables to bind.
        /// </summary>
        private SchemeObject vars;

        /// <summary>
        /// This list of initial expressions.
        /// </summary>
        private SchemeObject inits;

        /// <summary>
        /// Evaluated values of inits.
        /// </summary>
        private SchemeObject vals;

        /// <summary>
        /// The list of formal parameters to pass to the final lambda.
        /// This is built up from variable1, variable2, ...
        /// It is in the reverse order from the given list.
        /// </summary>
        private SchemeObject formals;
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
        private EvaluateLetStar(SchemeObject expr, Environment env, Evaluator caller, SchemeObject body, SchemeObject vars, SchemeObject inits, SchemeObject formals, SchemeObject vals)
            : base(evalInitStep, expr, env, caller, counter)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(body != null);
            Contract.Requires(vars != null);
            Contract.Requires(inits != null);
            Contract.Requires(formals != null);
            Contract.Requires(vals != null);
            Contract.Requires(counter >= 0);
            this.body = body;
            this.vars = vars;
            this.inits = inits;
            this.formals = formals;
            this.vals = vals;
        }
        #endregion

        #region Call
        /// <summary>
        /// Call let* evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The let evaluator.</returns>
        internal static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            if (expr is EmptyList)
            {
                ErrorHandlers.SemanticError("No arguments arguments for let*", null);
            }

            if (!(expr is Pair))
            {
                ErrorHandlers.SemanticError("Bad arg list for let*: " + expr, null);
            }

            SchemeObject bindings = First(expr);
            SchemeObject body = Rest(expr);

            if (body is EmptyList)
            {
                caller.ReturnedExpr = Undefined.Instance;
                return caller;
            }

            var vars = MapFun(First, bindings);
            var inits = MapFun(Second, bindings);
            var formals = EmptyList.Instance;
            var vals = EmptyList.Instance;
            return new EvaluateLetStar(expr, env, caller, body, vars, inits, formals, vals);
        }
        #endregion

        #region steps
        /// <summary>
        /// Evaluate one of the inits.
        /// Do it in an environment made up of the previously evaluated inits bound to their vars.
        /// </summary>
        /// <returns>The next step.</returns>
        protected override Evaluator EvalInitStep()
        {
            if (this.inits is EmptyList)
            {
                this.Pc = applyProcStep;
                return this;
            }

            Lambda fun = Lambda.New(this.formals, MakeList(First(this.inits)), this.Env);
            this.Pc = bindVarToInitStep;
            return fun.Apply(this.vals, this, this);
        }

        /// <summary>
        /// Add var to list of formals, evaluation result to list of vals.
        /// Move down list of vars and inits.
        /// Go back and evaluate another init.
        /// </summary>
        /// <returns>The next step.</returns>
        protected override Evaluator BindVarToInitStep()
        {
            this.formals = Cons(First(this.vars), this.formals);
            this.vals = Cons(this.ReturnedExpr, this.vals);
            this.vars = Rest(this.vars);
            this.inits = Rest(this.inits);
            this.Pc = evalInitStep;
            return this;
        }

        /// <summary>
        /// Inits evaluated and bound -- execute the proc.
        /// </summary>
        /// <returns>Execution continues with evaluation of the body of the let.</returns>
        protected override Evaluator ApplyProcStep()
        {
            // apply the fun to the vals
            Lambda fun = Lambda.New(this.formals, this.body, this.Env);
            return fun.Apply(this.vals, this.Caller, this);
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.body != null);
            Contract.Invariant(this.vars != null);
            Contract.Invariant(this.inits != null);
            Contract.Invariant(this.formals != null);
            Contract.Invariant(this.vals != null);
        }
        #endregion
    }
}