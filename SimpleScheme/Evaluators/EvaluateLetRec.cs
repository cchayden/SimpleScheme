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
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-letrec");

        /// <summary>
        /// The body of the let.
        /// </summary>
        private SchemeObject body;

        /// <summary>
        /// The list of formal parameters to pass to the final lambda.
        /// This is variable1, variable2, ...
        /// </summary>
        private SchemeObject formals;

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
                ErrorHandlers.SemanticError("No arguments for letrec");
            }

            if (!(expr is Pair))
            {
                ErrorHandlers.SemanticError("Bad arg list for letrec: " + expr);
            }

            SchemeObject body = Rest(expr);
            if (body is EmptyList)
            {
                caller.ReturnedExpr = Undefined.Instance;
                return caller;
            }

            SchemeObject bindings = First(expr);
            var formals = MapFun(First, bindings);
            var initVals = Fill(ListLength(formals), Undefined.Instance);

            return New(expr, new Environment(env, formals, initVals), caller);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Evaluate one of the inits in the environment of the vars.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator EvalInitStep()
        {
            if (this.inits is EmptyList)
            {
                // now do ApplyProc
                return this.ApplyProcStep();
            }

            var fun = new Lambda(this.formals, MakeList(First(this.inits)), this.Env);
            this.Pc = OpCode.BindVarToInit;
            return fun.ApplyWithGivenEnv(this.Env, this);
        }

        /// <summary>
        /// Add var to list of formals, evaluation result to list of vals.
        /// Move down list of vars and inits.
        /// Go back and evaluate another init.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator BindVarToInitStep()
        {
            this.vals = Cons(this.ReturnedExpr, this.vals);
            this.vars = Rest(this.vars);
            this.inits = Rest(this.inits);

            // now do EvalInit
            return this.EvalInitStep();
        }

        /// <summary>
        /// Inits evaluated and bound -- execute the proc and return.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator ApplyProcStep()
        {
            // assign the inits into the env
            this.UpdateEnvironment(this.formals, ReverseList(this.vals));

            // apply the fun to the vals and return
            var fun = new Lambda(this.formals, this.body, this.Env);
            Environment ev = this.Env;
            Evaluator c = this.Caller;
            this.Reclaim();
            return fun.ApplyWithGivenEnv(ev, c);
        }
        #endregion

        #region Initialize
        /// <summary>
        /// Creates and initializes a new instance of the EvaluateLetRec class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Initialized evaluator.</returns>
        private static EvaluateLetRec New(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return GetInstance<EvaluateLetRec>().Initialize(expr, env, caller);
        }

        /// <summary>
        /// Initializes a new instance of the EvaluateLetRec class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Initialized evaluator.</returns>
        private EvaluateLetRec Initialize(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(counter >= 0);
            SchemeObject bindings = First(expr);
            //// In the bindings, the variables are first, the values are second, and anything left over is discarded.
            //// If either is missing, an empty list is used instead.
            //// To start with, bindings are undefined.
            this.body = Rest(expr);
            this.formals = MapFun(First, bindings);
            this.vars = formals;
            this.inits = MapFun(Second, bindings);
            this.vals = EmptyList.Instance;
            Initialize(OpCode.EvalInit, expr, env, caller, counter);
            return this;
        }
        #endregion
        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.degenerate || this.body != null);
            Contract.Invariant(this.degenerate || this.vars != null);
            Contract.Invariant(this.degenerate || this.inits != null);
            Contract.Invariant(this.degenerate || this.formals != null);
            Contract.Invariant(this.degenerate || this.vals != null);
        }
        #endregion
    }
}