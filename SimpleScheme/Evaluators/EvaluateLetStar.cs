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
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-let*");

        /// <summary>
        /// The body of the let.
        /// </summary>
        private SchemeObject body;

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
                ErrorHandlers.SemanticError("No arguments arguments for let*");
            }

            if (!(expr is Pair))
            {
                ErrorHandlers.SemanticError("Bad arg list for let*: " + expr);
            }

            SchemeObject body = Rest(expr);
            if (body is EmptyList)
            {
                caller.ReturnedExpr = Undefined.Instance;
                return caller;
            }

            return New(expr, env, caller);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Evaluate one of the inits.
        /// Do it in an environment made up of the previously evaluated inits bound to their vars.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator EvalInitStep()
        {
            if (this.inits is EmptyList)
            {
                // do ApplyProc now
                return this.ApplyProcStep();
            }

            var fun = new Lambda(this.formals, MakeList(First(this.inits)), this.Env);
            this.Pc = OpCode.BindVarToInit;
            return fun.Apply(this.vals, this);
        }

        /// <summary>
        /// Add var to list of formals, evaluation result to list of vals.
        /// Move down list of vars and inits.
        /// Go back and evaluate another init.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator BindVarToInitStep()
        {
            this.formals = Cons(First(this.vars), this.formals);
            this.vals = Cons(this.ReturnedExpr, this.vals);
            this.vars = Rest(this.vars);
            this.inits = Rest(this.inits);

            // finish up by doing EvalInit
            return this.EvalInitStep();
        }

        /// <summary>
        /// Inits evaluated and bound -- execute the proc.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator ApplyProcStep()
        {
            // apply the fun to the vals
            var fun = new Lambda(this.formals, this.body, this.Env);
            var v = this.vals;
            var c = this.Caller;
            this.Reclaim();
            return fun.Apply(v, c);
        }
        #endregion

        #region Initialize
        /// <summary>
        /// Creates and initializes a new instance of the EvaluateLetStar class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Initialized evaluator.</returns>
        private static EvaluateLetStar New(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return GetInstance<EvaluateLetStar>().Initialize(expr, env, caller);
        }

        /// <summary>
        /// Initializes a new instance of the EvaluateLetStar class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Initialized evaluator.</returns>
        private EvaluateLetStar Initialize(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(counter >= 0);
            SchemeObject bindings = First(expr);
            this.body = Rest(expr);
            this.vars = MapFun(First, bindings);
            this.inits = MapFun(Second, bindings);
            this.formals = EmptyList.Instance;
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