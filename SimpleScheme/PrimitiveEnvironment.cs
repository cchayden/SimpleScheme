// <copyright file="PrimitiveEnvironment.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// The primitive environment is "below" the global environment.
    /// It is the only environment allowed to have primitives.
    /// It is separate from the global environment so that it can be shared 
    ///   between interpreter instances.
    /// Normally, this should be set up at the beginning and not modified.  There
    ///   is no provision for thread safety because it is expected to be read-only.
    /// </summary>
    public class PrimitiveEnvironment : Environment, IPrimitiveEnvironment
    {
        #region Constructor
        /// <summary>
        /// Initializes a new instance of the PrimitiveEnvironment class.
        /// </summary>
        internal PrimitiveEnvironment() :
            base(NullInterp, Empty)
        {
            this.InstallPrimitives();
        }
        #endregion

        #region Factory Methods
        /// <summary>
        /// Creates a new primitive environment.
        /// </summary>
        /// <returns>A new primitive environment.</returns>
        public static IPrimitiveEnvironment New()
        {
            return new PrimitiveEnvironment();
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Define a primitive, taking a variable number of arguments.
        /// Creates a Primitive and puts it in the environment associated 
        ///    with the given name.
        /// </summary>
        /// <param name="name">The primitive name.</param>
        /// <param name="operation">The operation to perform.</param>
        /// <param name="minArgs">The minimum number of arguments.</param>
        /// <param name="maxArgs">The maximum number of arguments.</param>
        /// <returns>A refernce to the environment.</returns>
        public IPrimitiveEnvironment DefinePrimitive(Obj name, Primitive.Op operation, int minArgs, int maxArgs)
        {
            this.UnsafeDefine(name, new Primitive(operation, minArgs, maxArgs));
            return this;
        }

        /// <summary>
        /// Define a primitive, taking a fixed number of arguments.
        /// Creates a Primitive and puts it in the environment associated 
        ///    with the given name.
        /// </summary>
        /// <param name="name">The primitive name.</param>
        /// <param name="operation">The operation to perform.</param>
        /// <param name="numberOfArgs">The number of arguments.</param>
        /// <returns>A refernce to the environment.</returns>
        public IPrimitiveEnvironment DefinePrimitive(Obj name, Primitive.Op operation, int numberOfArgs)
        {
            this.UnsafeDefine(name, new Primitive(operation, numberOfArgs, numberOfArgs));
            return this;
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Install standard primitives into the environment.
        /// </summary>
        private void InstallPrimitives()
        {
            EvaluateExpression.DefinePrimitives(this);
            Number.DefinePrimitives(this);
            Procedure.DefinePrimitives(this);
            List.DefinePrimitives(this);
            InputPort.DefinePrimitives(this);
            OutputPort.DefinePrimitives(this);
            Vector.DefinePrimitives(this);
            SchemeBoolean.DefinePrimitives(this);
            SchemeString.DefinePrimitives(this);
            Character.DefinePrimitives(this);
            Symbol.DefinePrimitives(this);
            ClrProcedure.DefinePrimitives(this);
            ClrConstructor.DefinePrimitives(this);
            SynchronousClrProcedure.DefinePrimitives(this);
            AsynchronousClrProcedure.DefinePrimitives(this);
            Counter.DefinePrimitives(this);
            Interpreter.DefinePrimitives(this);
            ErrorHandlers.DefinePrimitives(this);

            this
                .DefinePrimitive(
                    "exit",
                    (args, caller) =>
                    {
                        System.Environment.Exit(EmptyList.Is(List.First(args)) ? 0 : (int)Number.As(List.First(args)));
                        return new Undefined();
                    },
                    0,
                    1)
                .DefinePrimitive("time-call", (args, caller) => EvaluateTimeCall.Call(args, caller.Env, caller), 1, 2);
        }
        #endregion
    }
}