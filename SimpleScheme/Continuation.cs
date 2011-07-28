// <copyright file="Continuation.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Represents a continuation.
    /// </summary>
    public sealed class Continuation : Procedure
    {
        #region Fields
        /// <summary>
        /// The step to execute when the continuation is applied.
        /// </summary>
        private readonly Stepper step;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Continuation class.
        /// The step and its chain of steps back to the beginning have to be cloned because they
        ///   hold information about the progress of the evaluation.  When the evaluation proceeds
        ///   these steps might be altered, damaging the ability to continue, which is what makes the
        ///   clone necessary.
        /// </summary>
        /// <param name="step">The continuation to return to when applied.</param>
        public Continuation(Stepper step)
        {
            this.step = step.Caller.CloneChain(); 
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Tests whether to given object is a scheme continuation.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme continuation.</returns>
        public static bool IsContinuation(Obj obj)
        {
            return obj is Continuation;
        }

        /// <summary>
        /// Convert an object to a continuation.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The continuation.</returns>
        public static Continuation AsContinuation(Obj obj)
        {
            return (Continuation)obj;
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
        /// Transfers execution to the step saved when the continuation was created.
        /// The environment in effect at that time is also restored.
        /// Again, the chain of steps back to the beginning need to be clones so that this application
        ///   does not alter the evaluation, making it impossible to return back to the continuation.
        /// </summary>
        /// <param name="args">The value to return.</param>
        /// <param name="caller">The calling evaluator.  Not used, since control is transferred away.</param>
        /// <returns>The next step to execute.</returns>
        public override Stepper Apply(object args, Stepper caller)
        {
            return Stepper.TransferToStep(this.step.CloneChain(), List.First(args), this.step.Env);
        }
        #endregion
    }

    #region Extensions
    /// <summary>
    /// Provide common operations as extensions.
    /// </summary>
    public static partial class Extensions
    {
        /// <summary>
        /// Write the continuation to the string builder.
        /// </summary>
        /// <param name="cont">The continuation (not used).</param>
        /// <param name="quoted">Whether to quote (not used).</param>
        /// <param name="buf">The string builder to write to.</param>
        public static void AsString(this Continuation cont, bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append("<continuation>");
            }
        }
    }
    #endregion
}
