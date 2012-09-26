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
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper initialStep = GetStepper("InitialStep");

        /// <summary>
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper testStep = GetStepper("TestStep");

        /// <summary>
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper iterateStep = GetStepper("IterateStep");

        /// <summary>
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper loopStep = GetStepper("LoopStep");

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-do");

        /// <summary>
        /// The list of variables to bind.
        /// </summary>
        private readonly SchemeObject vars;

        /// <summary>
        /// The list of initializers.
        /// </summary>
        private readonly SchemeObject inits;

        /// <summary>
        /// This list of step expressions.
        /// </summary>
        private readonly SchemeObject steps;

        /// <summary>
        /// The expression list following the test
        /// </summary>
        private readonly SchemeObject exprs;

        /// <summary>
        /// The commands to execute each time through.
        /// </summary>
        private readonly SchemeObject commands;

        /// <summary>
        /// The test proc to execute each time around.
        /// </summary>
        private readonly Lambda testProc;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateDo class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="vars">The variables.</param>
        /// <param name="inits">The initialization expressions.</param>
        /// <param name="steps">The steps.</param>
        /// <param name="exprs">The expressions.</param>
        /// <param name="commands">The commands.</param>
        /// <param name="testProc">The test proc to execute each interation.</param>
        private EvaluateDo(SchemeObject expr, Environment env, Evaluator caller, SchemeObject vars, SchemeObject inits, SchemeObject steps, SchemeObject exprs, SchemeObject commands, Lambda testProc)
            : base(initialStep, expr, new Environment(env), caller, counter)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(vars != null);
            Contract.Requires(inits != null);
            Contract.Requires(steps != null);
            Contract.Requires(exprs != null);
            Contract.Requires(commands != null);
            Contract.Requires(testProc != null);
            Contract.Requires(counter >= 0);
            this.vars = vars;
            this.inits = inits;
            this.steps = steps;
            this.exprs = exprs;
            this.commands = commands;
            this.testProc = testProc;
        }
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
                ErrorHandlers.SemanticError("No body for do", null);
            }

            if (!(expr is Pair))
            {
                ErrorHandlers.SemanticError("Bad arg list for do: " + expr, null);
            }

            var bindings = First(expr);
            var vars = MapFun(First, bindings);
            var inits = MapFun(Second, bindings);
            var steps = MapFun(ThirdOrFirst, bindings);
            var exprs = Rest(Second(expr));
            var commands = Rest(Rest(expr));

            SchemeObject test = First(Second(expr));
            if (test is EmptyList)
            {
                caller.ReturnedExpr = Undefined.Instance;
                return caller;
            }

            // prepare test proc to execute each time through
            var testProc = Lambda.New(vars, MakeList(test), env);
            return new EvaluateDo(expr, env, caller, vars, inits, steps, exprs, commands, testProc);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Start by evaluating the inits.
        /// </summary>
        /// <returns>Continues by evaluating the inits.</returns>
        protected override Evaluator InitialStep()
        {
            this.Pc = testStep;
            return EvaluateList.Call(this.inits, this.Env, this);
        }

        /// <summary>
        /// Evaluate the test expr in the environment containing the variables with their new values.
        /// These are the init values or the step values.
        /// </summary>
        /// <returns>The next step, which tests the result.</returns>
        protected override Evaluator TestStep()
        {
            this.UpdateEnvironment(this.vars, this.ReturnedExpr);
            this.Pc = iterateStep;
            return this.testProc.ApplyWithGivenEnv(this.Env, this);
        }

        /// <summary>
        /// Iterate: if test is true, eval the exprs and return.
        /// Otherwise, evaluate the commands.
        /// </summary>
        /// <returns>The next step.</returns>
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
                    Evaluator caller = this.Caller;
                    caller.ReturnedExpr = Undefined.Instance;
                    return caller;
                }

                return EvaluateSequence.Call(this.exprs, this.Env, this.Caller);
            }
            
            // test is false
            // evaluate the steps in the environment of the vars
            // bind to fresh copies of the vars
            this.Pc = loopStep;
            return EvaluateList.Call(this.commands, this.Env, this);
        }

        /// <summary>
        /// Evaluate the step expressions.  
        /// </summary>
        /// <returns>The next step.</returns>
        protected override Evaluator LoopStep()
        {
            this.Pc = testStep;
            return EvaluateList.Call(this.steps, this.Env, this);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Extract the steps (third element in list).
        /// If it is missing, then use the first instead.
        /// </summary>
        /// <param name="x">The list to start with.</param>
        /// <returns>The third, if it exists, otherwise the first.</returns>
        private static SchemeObject ThirdOrFirst(SchemeObject x)
        {
            Contract.Requires(x != null);
            SchemeObject res = Third(x);
            return res is EmptyList ? First(x) : res;
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.vars != null);
            Contract.Invariant(this.inits != null);
            Contract.Invariant(this.steps != null);
            Contract.Invariant(this.exprs != null);
            Contract.Invariant(this.commands != null);
            Contract.Invariant(this.testProc != null);
        }
        #endregion
    }
}