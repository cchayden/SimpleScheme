// <copyright file="EvaluateDo.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
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
            : base(InitialStep, expr, new Environment(env), caller)
        {
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

            // Test for errors before creating the object.
            if (expr is EmptyList)
            {
                ErrorHandlers.SemanticError("No body for do", null);
            }

            if (!(expr is Pair))
            {
                ErrorHandlers.SemanticError("Bad arg list for do: " + expr, null);
            }

            SchemeObject bindings = First(expr);
            SchemeObject vars = MapFun(First, bindings);
            SchemeObject inits = MapFun(Second, bindings);
            SchemeObject steps = MapFun(ThirdOrFirst, bindings);
            SchemeObject exprs = Rest(Second(expr));
            SchemeObject commands = Rest(Rest(expr));

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

        #region Private Methods
        /// <summary>
        /// Extract the steps (third element in list).
        /// If it is missing, then use the first instead.
        /// </summary>
        /// <param name="x">The list to start with.</param>
        /// <returns>The third, if it exists, otherwise the first.</returns>
        private static SchemeObject ThirdOrFirst(SchemeObject x)
        {
            SchemeObject res = Third(x);
            return res is EmptyList ? First(x) : res;
        }
        #endregion

        #region Steps
        /// <summary>
        /// Start by evaluating the inits.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Continues by evaluating the inits.</returns>
        private static Evaluator InitialStep(Evaluator s)
        {
            var step = (EvaluateDo)s;
            s.Pc = TestStep;
            return EvaluateList.Call(step.inits, s.Env, s);
        }

        /// <summary>
        /// Evaluate the test expr in the environment containing the variables with their new values.
        /// These are the init values or the step values.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next step, which tests the result.</returns>
        private static Evaluator TestStep(Evaluator s)
        {
            var step = (EvaluateDo)s;
            step.UpdateEnvironment(step.vars, s.ReturnedExpr);
            s.Pc = IterateStep;
            return step.testProc.ApplyWithtEnv(s.Env, s);
        }

        /// <summary>
        /// Iterate: if test is true, eval the exprs and return.
        /// Otherwise, evaluate the commands.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next step.</returns>
        private static Evaluator IterateStep(Evaluator s)
        {
            var step = (EvaluateDo)s;
            if (SchemeBoolean.Truth(step.ReturnedExpr).Value)
            {
                // test is true
                // Evaluate exprs and return the value of the last
                //   in the environment of the vars.
                // If no exprs, unspecified.
                if (step.exprs is EmptyList)
                {
                    Evaluator caller = step.Caller;
                    caller.ReturnedExpr = Undefined.Instance;
                    return caller;
                }

                return EvaluateSequence.Call(step.exprs, s.Env, step.Caller);
            }
            
            // test is false
            // evaluate the steps in the environment of the vars
            // bind to fresh copies of the vars
            s.Pc = LoopStep;
            return EvaluateList.Call(step.commands, s.Env, s);
        }

        /// <summary>
        /// Evaluate the step expressions.  
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next step.</returns>
        private static Evaluator LoopStep(Evaluator s)
        {
            var step = (EvaluateDo)s;
            s.Pc = TestStep;
            return EvaluateList.Call(step.steps, s.Env, s);
        }
        #endregion
    }
}