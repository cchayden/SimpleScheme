﻿// <copyright file="EvaluateDo.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate a do expression
    /// Bind the variables to values and then evaluate the expressions.
    /// </summary>
    //// <r4rs section="4.2.4">(do ((variable1> <init1> <step1>) 
    ////                           ...)
    ////                           (<test> <expression> ...)
    ////                         <command> ...)</r4rs>
    public sealed class EvaluateDo : Evaluator
    {
        #region Fields
        /// <summary>
        /// The name of the evaluator, used for counters and tracing.
        /// </summary>
        public const string EvaluatorName = "evaluate-do";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(EvaluatorName);

        /// <summary>
        /// The list of variables to bind.
        /// </summary>
        private readonly Obj vars;

        /// <summary>
        /// The list of initializers.
        /// </summary>
        private readonly Obj inits;

        /// <summary>
        /// This list of step expressions.
        /// </summary>
        private readonly Obj steps;


        /// <summary>
        /// The expression list following the test
        /// </summary>
        private readonly Obj exprs;

        /// <summary>
        /// The commands to execute each time through.
        /// </summary>
        private readonly Obj commands;

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
        private EvaluateDo(Obj expr, Environment env, Evaluator caller, Obj vars, Obj inits, Obj steps, Obj exprs, Obj commands, Lambda testProc)
            : base(expr, env, caller)
        {
            this.vars = vars;
            this.inits = inits;
            this.steps = steps;
            this.exprs = exprs;
            this.commands = commands;
            this.testProc = testProc;
            ContinueHere(InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Public Static Methods
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
        public static Evaluator Call(Obj expr, Environment env, Evaluator caller)
        {
            // Test for errors before creating the object.
            if (EmptyList.Is(expr))
            {
                ErrorHandlers.SemanticError("No body for do");
                return caller.UpdateReturnValue(Undefined.Instance);
            }

            if (!Pair.Is(expr))
            {
                ErrorHandlers.SemanticError("Bad arg list for do: " + expr);
                return caller.UpdateReturnValue(Undefined.Instance);
            }

            Obj bindings = List.First(expr);
            Obj vars = List.MapFun(List.First, List.New(bindings));
            Obj inits = List.MapFun(List.Second, List.New(bindings));
            Obj steps = List.MapFun(ThirdOrFirst, List.New(bindings));
            Obj exprs = List.Rest(List.Second(expr));
            Obj commands = List.Rest(List.Rest(expr));

            Obj test = List.First(List.Second(expr));
            if (EmptyList.Is(test))
            {
                return caller.UpdateReturnValue(Undefined.Instance);
            }

            // prepare test proc to execute each time through
            Lambda testProc = new Lambda(vars, List.New(test), env);
            EvaluateDo eval = new EvaluateDo(expr, env, caller, vars, inits, steps, exprs, commands, testProc);

            // push an empty environment, to hold the iteration variables
            eval.PushEmptyEnvironment(env);
            return eval;
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Extract the steps (third element in list).
        /// If it is missing, then use the first instead.
        /// </summary>
        /// <param name="x">The list to start with.</param>
        /// <returns>The third, if it exists, otherwise the first.</returns>
        private static Obj ThirdOrFirst(Obj x)
        {
            Obj res = List.Third(x);
            return EmptyList.Is(res) ? List.First(x) : res;
        }

        /// <summary>
        /// Start by evaluating the inits.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Continues by evaluating the inits.</returns>
        private static Evaluator InitialStep(Evaluator s)
        {
            EvaluateDo step = (EvaluateDo)s;
            return EvaluateList.Call(step.inits, s.Env, s.ContinueHere(TestStep));
        }

        /// <summary>
        /// Evaluate the test expr in the environment containing the variables with their new values.
        /// These are the init values or the step values.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next step, which tests the result.</returns>
        private static Evaluator TestStep(Evaluator s)
        {
            EvaluateDo step = (EvaluateDo)s;
            s.ReplaceEnvironment(step.vars, s.ReturnedExpr, s.Env);
            return step.testProc.ApplyWithtEnv(s.Env, step.ContinueHere(IterateStep));
        }

        /// <summary>
        /// Iterate: if test is true, eval the exprs and return.
        /// Otherwise, evaluate the commands.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next step.</returns>
        private static Evaluator IterateStep(Evaluator s)
        {
            EvaluateDo step = (EvaluateDo)s;
            if (SchemeBoolean.Truth(step.ReturnedExpr))
            {
                // test is true
                // Evaluate exprs and return the value of the last
                //   in the environment of the vars.
                // If no exprs, unspecified.
                if (EmptyList.Is(step.exprs))
                {
                    return step.ReturnUndefined();
                }

                return EvaluateSequence.Call(step.exprs, step.Env, step.Caller);
            }
            
            // test is false
            // evaluate the steps in the environment of the vars
            // bind to fresh copies of the vars
            return EvaluateList.Call(step.commands, step.Env, step.ContinueHere(LoopStep));
        }

        /// <summary>
        /// Evaluate the step expressions.  
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next step.</returns>
        private static Evaluator LoopStep(Evaluator s)
        {
            EvaluateDo step = (EvaluateDo)s;
            return EvaluateList.Call(step.steps, step.Env, step.ContinueHere(TestStep));
        }
        #endregion
    }
}