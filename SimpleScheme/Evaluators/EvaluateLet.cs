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
        private readonly Symbol name;

        /// <summary>
        /// The body of the let.
        /// </summary>
        private readonly SchemeObject body;

        /// <summary>
        /// The list of variables to bind.
        /// </summary>
        private readonly SchemeObject vars;

        /// <summary>
        /// This list of initial expressions.
        /// </summary>
        private readonly SchemeObject inits;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateLet class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="name">The name of a named let.</param>
        /// <param name="body">The let body.</param>
        /// <param name="vars">The variables to bind.</param>
        /// <param name="inits">The initial values of the variables.</param>
        private EvaluateLet(SchemeObject expr, Environment env, Evaluator caller, Symbol name, SchemeObject body, SchemeObject vars, SchemeObject inits)
            : base(OpCode.Initial, expr, env, caller, counter)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(counter >= 0);
            //// name is null except for named let
            Contract.Requires(body != null);
            Contract.Requires(vars != null);
            Contract.Requires(inits != null);
            this.name = name;
            this.body = body;
            this.vars = vars;
            this.inits = inits;
        }
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
                ErrorHandlers.SemanticError("No arguments for let", null);
                caller.ReturnedExpr = Undefined.Instance;
                return caller;
            }

            if (!(expr is Pair))
            {
                ErrorHandlers.SemanticError("Bad arg list for let: " + expr, null);
                caller.ReturnedExpr = Undefined.Instance;
                return caller;
            }

            Symbol name = null;
            SchemeObject bindings;
            SchemeObject body;
            if (First(expr) is Symbol)
            {
                // named let
                name = (Symbol)First(expr);   
                bindings = Second(expr);
                body = Rest(Rest(expr));
            }
            else
            {
                bindings = First(expr);
                body = Rest(expr);
            }

            if (body is EmptyList)
            {
                caller.ReturnedExpr = Undefined.Instance;
                return caller;
            }

            var vars = MapFun(First, bindings);
            var inits = MapFun(Second, bindings);
            return new EvaluateLet(expr, env, caller, name, body, vars, inits);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Convert the let to a corresponding lambda.
        /// For a normal let, evaluate this lambda.
        /// For a named let, construct the additional lambda to bind to the name.
        /// </summary>
        /// <returns>Continues by evaluating the constructed lambda.</returns>
        protected override Evaluator InitialStep()
        {
            if (this.name == null)
            {
                // regular let -- create a lambda for the body, bind inits to it, and apply it
                return EvaluateProc.Call(Lambda.New(this.vars, this.body, this.Env), this.inits, this.Env, this.Caller);
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
        /// <returns>The next evaluator to execute.</returns>
        protected override Evaluator ApplyNamedLetStep()
        {
            Contract.Assume(this.name != null);
            Lambda fn = Lambda.New(this.vars, this.body, this.Env);
            fn.Env.Define(this.name, fn);   
            return fn.Apply(this.ReturnedExpr, this.Caller, this);
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
        }
        #endregion
    }
}