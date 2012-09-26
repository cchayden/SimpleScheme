// <copyright file="Continuation.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Represents a continuation.
    /// Continuations are immutable.
    /// </summary>
    public sealed class Continuation : Procedure
    {
        #region Constants
        /// <summary>
        /// The name of the evaluator, used for counters and tracing.
        /// </summary>
        public new const string Name = "continuation";
        #endregion

        /// <summary>
        /// The printable name of this scheme type.
        /// </summary>
        public new static string TypeName = Primitive.ValueType.Continuation.ToString();

        /// <summary>
        /// Identifies objects of this scheme type.
        /// </summary>
        /// <param name="obj">The object to test.</param>
        /// <returns>True if the object is this scheme type.</returns>
        public new static bool Is(Obj obj)
        {
            return obj is Continuation;
        }

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
            base(1, 1)
        {
            this.savedEvaluator = eval.CloneChain(); 
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Creates a new instance of the Continuation class.
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
        /// Write the continuation to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote (not used).</param>
        /// <param name="buf">The string builder to write to.</param>
        public new void PrintString(bool quoted, StringBuilder buf)
        {
            buf.Append("<" + Name + ">");
        }

        /// <summary>
        /// Display the continuation as a string.  
        /// Displays the body, as it has been processed by the reader.
        /// </summary>
        /// <returns>The string form of the continuation.</returns>
        public override string ToString()
        {
            return string.Empty;
        }

        /// <summary>
        /// Execute the continuation.
        /// Transfers execution to the evaluator saved when the continuation was created.
        /// The environment in effect at that time is also restored.
        /// Again, the chain of steps back to the beginning need to be clones so that this application
        ///   does not alter the evaluation, making it impossible to return back to the continuation.
        /// </summary>
        /// <param name="args">The value to return.</param>
        /// <param name="caller">The calling evaluator.  Not used, since control is transferred away.</param>
        /// <returns>The next evaluator to execute.</returns>
        public override Evaluator Apply(Obj args, Evaluator caller)
        {
            CheckArgs(args, "Continuation");
            return Evaluator.TransferToStep(this.savedEvaluator.CloneChain(), args.First(), this.savedEvaluator.Env);
        }
        #endregion
    }

    #region Extension Class
    /// <summary>
    /// Extensions for Continuation
    /// </summary>
    public static class ContinuationExtension
    {
        /// <summary>
        /// Tests whether to given object is a scheme continuation.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme continuation.</returns>
        public static bool IsContinuation(this Obj obj)
        {
            return Continuation.Is(obj);
        }

        /// <summary>
        /// Convert an object to a continuation.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The continuation.</returns>
        public static Continuation AsContinuation(Obj obj)
        {
            if (Continuation.Is(obj))
            {
                return (Continuation)obj;
            }

            ErrorHandlers.TypeError(Continuation.Name, obj);
            return null;
        }
    }
    #endregion
}
