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
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateDo(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
            this.Pc = this.InitialStep;
            IncrementCounter("do");
        }

        /// <summary>
        /// Call let evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The let evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
        {
            return new EvaluateDo(caller, expr, caller.Env);
        }

        /// <summary>
        /// Extract the steps (third element in list).
        /// If it is missing, then use the first instead.
        /// </summary>
        /// <param name="x">The list to start with.</param>
        /// <returns>The third, if it exists, otherwise the first.</returns>
        private static object ThirdOrFirst(object x)
        {
            return List.Third(x) ?? List.First(x);
        }

        /// <summary>
        /// Start by checking the number of arguments.
        /// Then test for named let.  
        /// Grab all the parts of the do statement.
        /// </summary>
        /// <returns>Continues by evaluating the inits.</returns>
        private Stepper InitialStep()
        {
            if (this.Expr == null)
            {
                ErrorHandlers.Error("Do: wrong number of arguments");
                return ReturnFromStep(null);
            }

            if (!(this.Expr is Pair))
            {
                ErrorHandlers.Error("Do: illegal arg list: " + this.Expr);
                return ReturnFromStep(null);
            }

            object bindings = List.First(this.Expr);
            this.vars = List.MapFun(List.First, List.MakeList(bindings));
            object inits = List.MapFun(List.Second, List.MakeList(bindings));
            this.steps = List.MapFun(ThirdOrFirst, List.MakeList(bindings));

            object test = List.First(List.Second(this.Expr));
            this.exprs = List.Rest(List.Second(this.Expr));
            this.commands = List.Rest(List.Rest(this.Expr));

            if (test == null)
            {
                return ReturnFromStep(null);
            }

            // prepare test proc to execute each time through
            this.testProc = new Closure(this.vars, List.MakeList(test), this.Env);

            // First evaluare inits.
            this.Pc = this.TestStep;
            return EvaluateList.Call(this, inits);
        }

        /// <summary>
        /// Evaluate the test expr in the environment agumented with the variables with their computed values.
        /// These are the init values or the step values.
        /// </summary>
        /// <returns>The next step, which tests the result.</returns>
        private Stepper TestStep()
        {
            this.Pc = this.IterateStep;
            this.Env = new Environment(this.vars, ReturnedExpr, this.Parent.Env);
            return this.testProc.ApplyWithEnv(this);
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
                this.Pc = this.ReturnStep;
                return EvaluateSequence.Call(this, this.exprs);
            }
            
            // test is false
            // evaluate the steps in the environment of the vars
            // bind to fresh copies of the vars
            this.Pc = this.LoopStep;
            return EvaluateList.Call(this, this.commands);
        }

        /// <summary>
        /// Evaluate the step expressions and loop back to the test.
        /// </summary>
        /// <returns>The next step.</returns>
        private Stepper LoopStep()
        {
            this.Pc = this.TestStep;
            return EvaluateList.Call(this, this.steps);
        }
    }
}