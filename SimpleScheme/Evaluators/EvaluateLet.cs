// <copyright file="EvaluateLet.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Evaluate a let expression
    /// Bind the variables to values and then evaluate the expressions.
    /// </summary>
    //// <r4rs section="4.2.2">(let <bindings> <body>)</r4rs>
    //// <r4rs section="4.2.4">(let <variable> <bindings> <body>)</r4rs>
    //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
    //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
    internal sealed class EvaluateLet : Evaluator
    {
        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-let");

        /// <summary>
        /// Name, for named let.
        /// </summary>
        private Symbol name;

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
        #endregion

        #region Call
        /// <summary>
        /// Call let evaluator.
        /// Start by checking the number of arguments.
        /// Then test for named let.  
        /// Grab bindings, body, and optionally name.
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
                ErrorHandlers.SemanticError("No arguments for let");
                caller.ReturnedExpr = Undefined.Instance;
                return caller;
            }

            if (!(expr is Pair))
            {
                ErrorHandlers.SemanticError("Bad arg list for let: " + expr);
                caller.ReturnedExpr = Undefined.Instance;
                return caller;
            }

            SchemeObject body;
            if (First(expr) is Symbol)
            {
                // named let
                body = Rest(Rest(expr));
            }
            else
            {
                body = Rest(expr);
            }

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
        /// Convert the let to a corresponding lambda.
        /// For a normal let, evaluate this lambda.
        /// For a named let, construct the additional lambda to bind to the name.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator InitialStep()
        {
            if (this.name == null)
            {
                // regular let -- create a lambda for the body, bind inits to it, and apply it
                var i = this.inits;
                var ev = this.Env;
                var c = this.Caller;
                this.Reclaim();
                return EvaluateProc.Call(new Lambda(this.vars, this.body, this.Env), i, ev, c);
            }

            // named let -- eval the inits in the outer environment
            this.Pc = OpCode.ApplyNamedLet;
            return EvaluateList.Call(this.inits, this.Env, this);
        }

        /// <summary>
        /// Apply the named let.
        /// Define a lambda with the bound vars and the body, and put it in the environment
        ///   of the lambda.
        /// Then apply the lambda.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator ApplyNamedLetStep()
        {
            Contract.Assume(this.name != null);
            var fn = new Lambda(this.vars, this.body, this.Env);
            fn.Env.Define(this.name, fn);
            var r = this.ReturnedExpr;
            var c = this.Caller;
            this.Reclaim();
            return fn.Apply(r, c);
        }
        #endregion

        #region Initialize
        /// <summary>
        /// Creates and initializes a new instance of the EvaluateLet class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Initialized evaluator.</returns>
        private static EvaluateLet New(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return GetInstance<EvaluateLet>().Initialize(expr, env, caller);
        }

        /// <summary>
        /// Initializes a new instance of the EvaluateLet class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Initialized evaluator.</returns>
        private EvaluateLet Initialize(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(counter >= 0);

            SchemeObject bindings;
            if (First(expr) is Symbol)
            {
                // named let
                this.name = (Symbol)First(expr);   
                bindings = Second(expr);
                this.body = Rest(Rest(expr));
            }
            else
            {
                this.name = null;
                bindings = First(expr);
                this.body = Rest(expr);
            }

            this.vars = MapFun(First, bindings);
            this.inits = MapFun(Second, bindings);
            Initialize(OpCode.Initial, expr, env, caller, counter);
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
        }
        #endregion
    }
}