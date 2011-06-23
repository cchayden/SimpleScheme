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
    public sealed class EvaluateDo : Stepper
    {
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "evaluate-do";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// The list of variables to bind.
        /// </summary>
        private object vars;

        /// <summary>
        /// This list of step expressions.
        /// </summary>
        private object steps;

        /// <summary>
        /// The expression list following the test
        /// </summary>
        private object exprs;

        /// <summary>
        /// The commands to execute each time through.
        /// </summary>
        private object commands;

        /// <summary>
        /// The test proc to execute each time around.
        /// </summary>
        private Closure testProc;

        /// <summary>
        /// Initializes a new instance of the EvaluateDo class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateDo(object expr, Environment env, Stepper caller)
            : base(caller, expr, env)
        {
            ContinueHere(this.InitialStep);
            IncrementCounter(counter);
        }

        /// <summary>
        /// Gets the name of the stepper.
        /// </summary>
        public override string Name
        {
            get { return StepperName; }
        }

        /// <summary>
        /// Call let evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The let evaluator.</returns>
        public static Stepper Call(object expr, Stepper caller)
        {
            return new EvaluateDo(expr, caller.Env, caller);
        }

        /// <summary>
        /// Extract the steps (third element in list).
        /// If it is missing, then use the first instead.
        /// </summary>
        /// <param name="x">The list to start with.</param>
        /// <returns>The third, if it exists, otherwise the first.</returns>
        private static object ThirdOrFirst(object x)
        {
            object res = Third(x);
            return res == List.Empty ? First(x) : res;
        }

        /// <summary>
        /// Start by checking the number of arguments.
        /// Then test for named let.  
        /// Grab all the parts of the do statement.
        /// </summary>
        /// <returns>Continues by evaluating the inits.</returns>
        private Stepper InitialStep()
        {
            if (Expr == List.Empty)
            {
                ErrorHandlers.Error("Do: wrong number of arguments");
                return ReturnFromStep(Undefined.Instance);
            }

            if (!(Expr is Pair))
            {
                ErrorHandlers.Error("Do: illegal arg list: " + Expr);
                return ReturnFromStep(Undefined.Instance);
            }

            object bindings = First(Expr);
            this.vars = MapFun(First, MakeList(bindings));
            object inits = MapFun(Second, MakeList(bindings));
            this.steps = MapFun(ThirdOrFirst, MakeList(bindings));

            object test = First(Second(Expr));
            this.exprs = Rest(Second(Expr));
            this.commands = Rest(Rest(Expr));

            if (test == List.Empty)
            {
                return ReturnFromStep(Undefined.Instance);
            }

            // prepare test proc to execute each time through
            this.testProc = new Closure(this.vars, MakeList(test), this.Env);

            // First evaluare inits.
            return EvaluateList.Call(ContinueHere(this.TestStep), inits);
        }

        /// <summary>
        /// Evaluate the test expr in the environment agumented with the variables with their computed values.
        /// These are the init values or the step values.
        /// </summary>
        /// <returns>The next step, which tests the result.</returns>
        private Stepper TestStep()
        {
            this.ReplaceEnvironment(this.vars, ReturnedExpr, this.Caller.Env);
            return this.testProc.ApplyWithCurrentEnv(ContinueHere(this.IterateStep));
        }

        /// <summary>
        /// Iterate: if test is true, eval the exprs and return.
        /// Otherwise, evaluate the commands.
        /// </summary>
        /// <returns>The next step.</returns>
        private Stepper IterateStep()
        {
            if (SchemeBoolean.Truth(ReturnedExpr))
            {
                // test is true
                // Evaluate exprs and return the value of the last
                //   in the environment of the vars.
                // If no exprs, unspecified.
                if (this.exprs == List.Empty)
                {
                    return ReturnFromStep(Undefined.Instance);
                }

                return EvaluateSequence.Call(this.exprs, ContinueReturn());
            }
            
            // test is false
            // evaluate the steps in the environment of the vars
            // bind to fresh copies of the vars
            return EvaluateList.Call(ContinueHere(this.LoopStep), this.commands);
        }

        /// <summary>
        /// Evaluate the step expressions and loop back to the test.
        /// </summary>
        /// <returns>The next step.</returns>
        private Stepper LoopStep()
        {
            return EvaluateList.Call(ContinueHere(this.TestStep), this.steps);
        }
    }
}