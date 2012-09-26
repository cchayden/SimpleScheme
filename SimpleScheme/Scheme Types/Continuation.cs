// <copyright file="Continuation.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;

    /// <summary>
    /// Represents a continuation.
    /// Continuations are immutable.
    /// </summary>
    internal sealed class Continuation : Procedure
    {
        #region Fields
        /// <summary>
        /// The evaluator to execute when the continuation is applied.
        /// </summary>
        private readonly Evaluator savedEvaluator;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Continuation class.
        /// The evaluator and its chain of evaluators back to the beginning have to be cloned because they
        ///   hold information about the progress of the evaluation.  When the evaluation proceeds
        ///   these evaluators might be altered, damaging the ability to continue, which is what makes the
        ///   clone necessary.
        /// </summary>
        /// <param name="eval">The continuation to return to when applied.</param>
        private Continuation(Evaluator eval) : 
            base(null, new ArgsInfo(1, 1, false))
        {
            this.savedEvaluator = eval.CloneChain(); 
        }
        #endregion

        #region New
        /// <summary>
        /// Initializes a new instance of the Continuation class.
        /// The evaluator and its chain of evaluators back to the beginning have to be cloned because they
        ///   hold information about the progress of the evaluation.  When the evaluation proceeds
        ///   these evaluators might be altered, damaging the ability to continue, which is what makes the
        ///   clone necessary.
        /// </summary>
        /// <param name="eval">The continuation to return to when applied.</param>
        /// <returns>A new continuation.</returns>
        public static Continuation New(Evaluator eval)
        {
            return new Continuation(eval);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Display the continuation as a string.  
        /// Displays the body, as it has been processed by the reader.
        /// </summary>
        /// <returns>The string form of the continuation.</returns>
        public override string ToString()
        {
            return "<continuation>";
        }

        /// <summary>
        /// Execute the continuation.
        /// Transfers execution to the evaluator saved when the continuation was created.
        /// The environment in effect at that time is also restored.
        /// Again, the chain of steps back to the beginning need to be clones so that this application
        ///   does not alter the evaluation, making it impossible to return back to the continuation.
        /// </summary>
        /// <param name="args">The value to return.</param>
        /// <param name="env"></param>
        /// <param name="returnTo">The evaluator to return to.  This can be different from caller if this is the last step in evaluation</param>
        /// <param name="caller">The calling evaluator.  Not used, since control is transferred away.</param>
        /// <returns>The next evaluator to execute.</returns>
        internal override Evaluator Apply(SchemeObject args, Environment env, Evaluator returnTo, Evaluator caller)
        {
#if Check
            this.CheckArgCount(ListLength(args), args, "Continuation", caller);
#endif
            Evaluator nextStep = this.savedEvaluator.CloneChain();
            nextStep.ReturnedExpr = First(args);
            nextStep.ReturnedEnv = this.savedEvaluator.Env;
            return nextStep;
        }
        #endregion
    }
}
