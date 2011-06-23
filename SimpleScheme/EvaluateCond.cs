// <copyright file="EvaluateCond.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Reduce a conditiona;
    /// </summary>
    //// <r4rs section="4.2.1">(cond <clause1> <clause2> ... ()</r4rs>
    //// <r4rs section="4.2.1">clause: (<test> <expression>)</r4rs>
    //// <r4rs section="4.2.1">clause: (<test> => <recipient>)</r4rs>
    //// <r4rs section="4.2.1">else clause: (else <expression1> <expression2> ...)</r4rs>
    public sealed class EvaluateCond : Stepper
    {
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "evaluate-cond";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// The value of the test expr
        /// </summary>
        private object test;

        /// <summary>
        /// The list of clauses in the cond
        /// </summary>
        private object clauses;

        /// <summary>
        /// The cond clause that is being processed.
        /// </summary>
        private object clause;

        /// <summary>
        /// Initializes a new instance of the EvaluateCond class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateCond(object expr, Environment env, Stepper caller)
            : base(caller, expr, env)
        {
            this.clauses = expr;
            ContinueHere(this.EvalClauseStep);
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
        /// Calls a cond evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The reduce cond evaluator.</returns>
        public static Stepper Call(object expr, Stepper caller)
        {
            // If no expr, avoid creating an evaluator.
            if (expr == List.Empty)
            {
                return caller.ContinueStep(SchemeBoolean.False);
            }

            return new EvaluateCond(expr, caller.Env, caller);
        }

        /// <summary>
        /// Evaluates a clause.  This step starts by checking for special conditions
        ///   such as else or the end of the list.
        /// Most often it evaluates the first clause.
        /// </summary>
        /// <returns>Usually, the step to evaluate the first clause.</returns>
        private Stepper EvalClauseStep()
        {
            this.clause = First(this.clauses);
            if (First(this.clause) as string == "else")
            {
                this.test = List.Empty;
                return ContinueHere(this.EvalConsequentStep);
            }

            return EvaluateExpression.Call(First(this.clause), ContinueHere(this.TestClauseStep));
        }

        /// <summary>
        /// Come here after evaluating a clause.
        /// If it is true, then proceed to evaluate the consequent.  
        /// Otherwise, go back to initial step.
        /// The list was stepped down already in the previous step.
        /// </summary>
        /// <returns>The next step, either loop or finish.</returns>
        private Stepper TestClauseStep()
        {
            this.test = ReturnedExpr;
            if (SchemeBoolean.Truth(this.test))
            {
                return ContinueHere(this.EvalConsequentStep);
            }

            this.clauses = Rest(this.clauses);
            if (this.clauses == List.Empty)
            {
                return ReturnFromStep(Undefined.Instance);
            }

            return ContinueHere(this.EvalClauseStep);
        }

        /// <summary>
        /// Come here when we have found a consequent to evaluate.
        /// Handle the varous forms for conequent.
        /// Evaluate and return the consequent.
        /// </summary>
        /// <returns>Execution continues with the caller.</returns>
        private Stepper EvalConsequentStep()
        {
            if (Rest(this.clause) == List.Empty)
            {
                // no consequent: return the test as the result
                return ReturnFromStep(this.test);
            }

            if (Second(this.clause) as string == "=>")
            {
                // send to recipient -- first evaluate recipient
                return EvaluateExpression.Call(Third(this.clause), ContinueHere(this.ApplyRecipientStep));
            }

            // evaluate and return the sequence of expressions
            return EvaluateSequence.Call(Rest(this.clause), this.Caller);
        }

        /// <summary>
        /// Apply the recipient function to the value of the test
        /// </summary>
        /// <returns>The next step to execute.</returns>
        private Stepper ApplyRecipientStep()
        {
            return EvaluateProcQuoted.Call(Procedure.Proc(ReturnedExpr), MakeList(this.test), this.Caller);
        }
    }
}