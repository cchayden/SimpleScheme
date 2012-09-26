// <copyright file="EvaluateDo.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Evaluate a do expression
    /// Bind the variables to values and then evaluate the expressions.
    /// </summary>
    //// <r4rs section="4.2.4">(do ((variable1> <init1> <step1>) 
    ////                           ...)
    ////                           (<test> <expression> ...)
    ////                         <command> ...)</r4rs>
    internal sealed class EvaluateDo : Evaluator
    {
        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-do");

        /// <summary>
        /// The list of variables to bind.
        /// </summary>
        private SchemeObject vars;

        /// <summary>
        /// The list of initializers.
        /// </summary>
        private SchemeObject inits;

        /// <summary>
        /// This list of step expressions.
        /// </summary>
        private SchemeObject steps;

        /// <summary>
        /// The expression list following the test
        /// </summary>
        private SchemeObject exprs;

        /// <summary>
        /// The commands to execute each time through.
        /// </summary>
        private SchemeObject commands;

        /// <summary>
        /// The test proc to execute each time around.
        /// </summary>
        private Lambda testProc;
        #endregion

        #region Call
        /// <summary>
        /// Call do evaluator.
        /// Start by checking the number of arguments.
        /// Grab all the parts of the do statement.
        /// Prepare an empty environment for the bound variables.
        /// Then create the evaluator.
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

            // Test for errors before creating the object.
            if (expr is EmptyList)
            {
                ErrorHandlers.SemanticError("No body for do");
            }

            if (!(expr is Pair))
            {
                ErrorHandlers.SemanticError("Bad arg list for do: " + expr);
            }

            SchemeObject test = First(Second(expr));
            if (test is EmptyList)
            {
                caller.ReturnedExpr = Undefined.Instance;
                return caller;
            }

            // prepare test proc to execute each time through
            return New(expr, env, caller);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Start by evaluating the inits.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator InitialStep()
        {
            this.Pc = OpCode.Test;
            return EvaluateList.Call(this.inits, this.Env, this);
        }

        /// <summary>
        /// Evaluate the test expr in the environment containing the variables with their new values.
        /// These are the init values or the step values.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator TestStep()
        {
            this.UpdateEnvironment(this.vars, this.ReturnedExpr);
            this.Pc = OpCode.Iterate;
            return this.testProc.ApplyWithGivenEnv(this.Env, this);
        }

        /// <summary>
        /// Iterate: if test is true, eval the exprs and return.
        /// Otherwise, evaluate the commands.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator IterateStep()
        {
            if (SchemeBoolean.Truth(this.ReturnedExpr).Value)
            {
                // test is true
                // Evaluate exprs and return the value of the last
                //   in the environment of the vars.
                // If no exprs, unspecified.
                if (this.exprs is EmptyList)
                {
                    return this.ReturnFromEvaluator(Undefined.Instance);
                }

                SchemeObject ex = this.exprs;
                Environment ev = this.Env;
                Evaluator c = this.Caller;
                this.Reclaim();
                return EvaluateSequence.Call(ex, ev, c);
            }
            
            // test is false
            // evaluate the steps in the environment of the vars
            // bind to fresh copies of the vars
            this.Pc = OpCode.Loop;
            return EvaluateList.Call(this.commands, this.Env, this);
        }

        /// <summary>
        /// Evaluate the step expressions.  
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator LoopStep()
        {
            this.Pc = OpCode.Test;
            return EvaluateList.Call(this.steps, this.Env, this);
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Extract the steps (third element in list).
        /// If it is missing, then use the first instead.
        /// </summary>
        /// <param name="x">The list to start with.</param>
        /// <returns>The third, if it exists, otherwise the first.</returns>
        /// <returns>The next step to execute.</returns>
        private static SchemeObject ThirdOrFirst(SchemeObject x)
        {
            Contract.Requires(x != null);
            SchemeObject res = Third(x);
            return res is EmptyList ? First(x) : res;
        }
        #endregion

        #region Initialize
        /// <summary>
        /// Creates and initializes a new instance of the EvaluateDo class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Initialized evaluator.</returns>
        private static EvaluateDo New(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return GetInstance<EvaluateDo>().Initialize(expr, env, caller);
        }

        /// <summary>
        /// Initializes a new instance of the EvaluateDo class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Newly initialized evaluator.</returns>
        private EvaluateDo Initialize(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(counter >= 0);
            var bindings = First(expr);
            this.vars = MapFun(First, bindings);
            this.inits = MapFun(Second, bindings);
            this.steps = MapFun(ThirdOrFirst, bindings);
            this.exprs = Rest(Second(expr));
            this.commands = Rest(Rest(expr));
            var test = First(Second(expr));
            this.testProc = new Lambda(vars, MakeList(test), env);
            Initialize(OpCode.Initial, expr, new Environment(env), caller, counter);
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
            Contract.Invariant(this.degenerate || this.vars != null);
            Contract.Invariant(this.degenerate || this.inits != null);
            Contract.Invariant(this.degenerate || this.steps != null);
            Contract.Invariant(this.degenerate || this.exprs != null);
            Contract.Invariant(this.degenerate || this.commands != null);
            Contract.Invariant(this.degenerate || this.testProc != null);
        }
        #endregion
    }
}