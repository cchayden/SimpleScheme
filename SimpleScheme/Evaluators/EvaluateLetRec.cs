// <copyright file="EvaluateLetRec.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Evaluate a letrec expression
    /// Bind the variables to values and then evaluate the expressions.
    /// </summary>
    //// <r4rs section="4.2.2">(letrec<bindings> <body>)</r4rs>
    //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
    //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
    internal sealed class EvaluateLetRec : Evaluator
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
        private static readonly int counter = Counter.Create("evaluate-letrec");

        /// <summary>
        /// The body of the let.
        /// </summary>
        private readonly SchemeObject body;

        /// <summary>
        /// The list of formal parameters to pass to the final lambda.
        /// This is variable1, variable2, ...
        /// </summary>
        private readonly SchemeObject formals;

        /// <summary>
        /// The list of variables to bind.
        /// </summary>
        private SchemeObject vars;

        /// <summary>
        /// This list of initial expressions.
        /// </summary>
        private SchemeObject inits;

        /// <summary>
        /// Accumulate the list of init vals here for later assignment.
        /// </summary>
        private SchemeObject vals;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateLetRec class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="body">The let body.</param>
        /// <param name="vars">The variables to be bound.</param>
        /// <param name="inits">The initialization expressions.</param>
        private EvaluateLetRec(SchemeObject expr, Environment env, Evaluator caller, SchemeObject body, SchemeObject vars, SchemeObject inits)
            : base(evalInitStep, expr, env, caller, counter)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(body != null);
            Contract.Requires(vars != null);
            Contract.Requires(inits != null);
            Contract.Requires(counter >= 0);
            this.body = body;
            this.vars = vars;
            this.inits = inits;
            this.formals = vars;
            this.vals = EmptyList.Instance;
        }
        #endregion

        #region Call
        /// <summary>
        /// Call letrec evaluator.
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
                ErrorHandlers.SemanticError("No arguments for letrec", null);
            }

            if (!(expr is Pair))
            {
                ErrorHandlers.SemanticError("Bad arg list for letrec: " + expr, null);
            }

            SchemeObject body = Rest(expr);
            if (body is EmptyList)
            {
                caller.ReturnedExpr = Undefined.Instance;
                return caller;
            }

            SchemeObject bindings = First(expr);
            //// In the bindings, the variables are first, the values are second, and anything left over is discarded.
            //// If either is missing, an empty list is used instead.
            //// To start with, bindings are undefined.
            var formals = MapFun(First, bindings);
            var inits = MapFun(Second, bindings);
            var initVals = Fill(ListLength(formals), Undefined.Instance);

            return new EvaluateLetRec(expr, new Environment(env, formals, initVals), caller, body, formals, inits);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Evaluate one of the inits in the environment of the vars.
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
            return fun.ApplyWithGivenEnv(this.Env, this);
        }

        /// <summary>
        /// Add var to list of formals, evaluation result to list of vals.
        /// Move down list of vars and inits.
        /// Go back and evaluate another init.
        /// </summary>
        /// <returns>The next step.</returns>
        protected override Evaluator BindVarToInitStep()
        {
            this.vals = Cons(this.ReturnedExpr, this.vals);
            this.vars = Rest(this.vars);
            this.inits = Rest(this.inits);
            this.Pc = evalInitStep;
            return this;
        }

        /// <summary>
        /// Inits evaluated and bound -- execute the proc and return.
        /// </summary>
        /// <returns>Execution returns to the caller.</returns>
        protected override Evaluator ApplyProcStep()
        {
            // assign the inits into the env
            this.UpdateEnvironment(this.formals, ReverseList(this.vals));

            // apply the fun to the vals and return
            Lambda fun = Lambda.New(this.formals, this.body, this.Env);
            return fun.ApplyWithGivenEnv(this.Env, this.Caller);
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