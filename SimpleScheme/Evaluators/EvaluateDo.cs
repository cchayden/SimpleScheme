// <copyright file="EvaluateDo.cs" company="Charles Hayden">
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
    public sealed class EvaluateDo : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        public const string StepperName = "evaluate-do";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// The list of variables to bind.
        /// </summary>
        private Obj vars;

        /// <summary>
        /// This list of step expressions.
        /// </summary>
        private Obj steps;

        /// <summary>
        /// The expression list following the test
        /// </summary>
        private Obj exprs;

        /// <summary>
        /// The commands to execute each time through.
        /// </summary>
        private Obj commands;

        /// <summary>
        /// The test proc to execute each time around.
        /// </summary>
        private Closure testProc;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateDo class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateDo(Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
            ContinueHere(InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Call let evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The let evaluator.</returns>
        public static Stepper Call(Obj expr, Environment env, Stepper caller)
        {
            return new EvaluateDo(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Extract the steps (third element in list).
        /// If it is missing, then use the first instead.
        /// </summary>
        /// <param name="x">The list to start with.</param>
        /// <returns>The third, if it exists, otherwise the first.</returns>
        private static Obj ThirdOrFirst(object x)
        {
            Obj res = List.Third(x);
            return EmptyList.IsEmptyList(res) ? List.First(x) : res;
        }

        /// <summary>
        /// Start by checking the number of arguments.
        /// Then test for named let.  
        /// Grab all the parts of the do statement.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Continues by evaluating the inits.</returns>
        private static Stepper InitialStep(Stepper s)
        {
            EvaluateDo step = (EvaluateDo)s;
            if (EmptyList.IsEmptyList(s.Expr))
            {
                ErrorHandlers.SemanticError("No body for do");
                return s.ReturnUndefined();
            }

            if (!Pair.IsPair(s.Expr))
            {
                ErrorHandlers.SemanticError("Bad arg list for do: " + s.Expr);
                return s.ReturnUndefined();
            }

            Obj bindings = List.First(s.Expr);
            step.vars = List.MapFun(List.First, List.New(bindings));
            Obj inits = List.MapFun(List.Second, List.New(bindings));
            step.steps = List.MapFun(ThirdOrFirst, List.New(bindings));

            Obj test = List.First(List.Second(s.Expr));
            step.exprs = List.Rest(List.Second(s.Expr));
            step.commands = List.Rest(List.Rest(s.Expr));

            if (EmptyList.IsEmptyList(test))
            {
                return s.ReturnUndefined();
            }

            // prepare test proc to execute each time through
            step.testProc = new Closure(step.vars, List.New(test), s.Env);

            // push an empty environment, to hold the iteration variables
            s.PushEmptyEnvironment(s.Env);

            // First evaluare inits.
            return EvaluateList.Call(inits, s.Env, s.ContinueHere(TestStep));
        }

        /// <summary>
        /// Evaluate the test expr in the environment containing the variables with their new values.
        /// These are the init values or the step values.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The next step, which tests the result.</returns>
        private static Stepper TestStep(Stepper s)
        {
            EvaluateDo step = (EvaluateDo)s;
            s.ReplaceEnvironment(step.vars, s.ReturnedExpr, s.Env);
            return step.testProc.ApplyWithtEnv(s.Env, step.ContinueHere(IterateStep));
        }

        /// <summary>
        /// Iterate: if test is true, eval the exprs and return.
        /// Otherwise, evaluate the commands.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The next step.</returns>
        private static Stepper IterateStep(Stepper s)
        {
            EvaluateDo step = (EvaluateDo)s;
            if (SchemeBoolean.Truth(step.ReturnedExpr))
            {
                // test is true
                // Evaluate exprs and return the value of the last
                //   in the environment of the vars.
                // If no exprs, unspecified.
                if (EmptyList.IsEmptyList(step.exprs))
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
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The next step.</returns>
        private static Stepper LoopStep(Stepper s)
        {
            EvaluateDo step = (EvaluateDo)s;
            return EvaluateList.Call(step.steps, step.Env, step.ContinueHere(TestStep));
        }
        #endregion
    }
}