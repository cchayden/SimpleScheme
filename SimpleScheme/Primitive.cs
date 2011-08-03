// <copyright file="Primitive.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Primitive procedures.
    /// This contains implementations for all primitive procedures.
    /// Each primitive knows its operation, a reference to the code to execute to carry out
    ///   the primitive.  It also knows the min and max number of arguments it expects.
    /// Each instance of Primitive is immutable.
    /// </summary>
    public sealed class Primitive : Procedure
    {
        #region Constants
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        public new const string Name = "primitive";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(Name);
        #endregion

        #region Fields
        /// <summary>
        /// The code to perform the operation.
        /// The stepper function is executed to perform the primitive operation.
        /// It takes two arguments, a caller and args.
        /// The caller is the step to return to when the operation is done.
        /// The args is the operand.
        /// The return value is either
        /// (1) a value, the operation result, or
        /// (2) a Stepper, the next step to execute. 
        /// </summary>
        private readonly Op operation;

        /// <summary>
        /// The minimum number of arguments expected.
        /// </summary>
        private readonly int minArgs;

        /// <summary>
        /// The maximum number of args expected.
        /// </summary>
        private readonly int maxArgs;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Primitive class.
        /// </summary>
        /// <param name="operation">The code to carry out the operation.</param>
        /// <param name="minArgs">The minimum number of arguments.</param>
        /// <param name="maxArgs">The maximum number of arguments.</param>
        public Primitive(Op operation, int minArgs, int maxArgs)
        {
            this.operation = operation;
            this.minArgs = minArgs;
            this.maxArgs = maxArgs;
        }
        #endregion

        #region Delegates
        /// <summary>
        /// The signature for primitives.
        /// </summary>
        /// <param name="args">The primitive's arguments</param>
        /// <param name="caller">The calling stepper.</param>
        /// <returns>The primitive's result.</returns>
        public delegate Obj Op(Obj args, Stepper caller);
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Tests whether to given object is a scheme primitive.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme primitive.</returns>
        public static bool IsPrimitive(Obj obj)
        {
            return obj is Primitive;
        }

        /// <summary>
        /// Convert an object to a primitive.
        /// </summary>
        /// <param name="obj">The primitive as an object.</param>
        /// <returns>The primitive.</returns>
        public static Primitive AsPrimitive(Obj obj)
        {
            return (Primitive)obj;
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// The string form of a proc is its name in curly brackets.
        /// </summary>
        /// <returns>The name of the proc.</returns>
        public override string ToString()
        {
            return "{" + this.ProcedureName + "}";
        }

        /// <summary>
        /// Apply the primitive to the arguments, giving a result.
        /// As a convenience for primitives, they are allowed to return either
        ///   a result or a Stepper.  If they return a stepper, it means the result
        ///   is not yet ready, and that a new stepper was created and returned.  When
        ///   that stepper has a result, it will put in into ReturnedResult and return to the
        ///   caller stepper provided to it.
        /// If there is a result available immediately, this returns it by storing it
        ///   in ReturnedResult and returning to the caller.
        /// </summary>
        /// <param name="args">The arguments to the primitive.</param>
        /// <param name="caller">The calling Stepper.</param>
        /// <returns>The next step to execute.</returns>
        public override Stepper Apply(object args, Stepper caller)
        {
            // First check the number of arguments
            int numArgs = List.Length(args);
            if (numArgs < this.minArgs)
            {
                return (Stepper)ErrorHandlers.SemanticError("Primitive: too few args, " + numArgs + ", for " +
                             this.ProcedureName + ": " + args);
            }

            if (numArgs > this.maxArgs)
            {
                return (Stepper)ErrorHandlers.SemanticError("Primitive: too many args, " + numArgs + ", for " +
                             this.ProcedureName + ": " + args);
            }

            caller.IncrementCounter(counter);

            // Execute the operation
            Obj res = this.operation(args, caller);

            // See if the operation returns a result or another step
            if (res is Stepper)
            {
                return (Stepper)res;
            }

            // Operation returned a result -- just return this
            //  to the caller.
            return caller.ContinueStep(res);
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
        /// Write the primitive to the string builder.
        /// </summary>
        /// <param name="prim">The primitive.</param>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public static void AsString(this Primitive prim, bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append("primitive: ");
                buf.Append(prim.ToString());
            }
        }
    }
    #endregion
}