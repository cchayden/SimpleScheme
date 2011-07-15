// <copyright file="PrimitiveEnvironment.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// The primitive environment is "below" the global environment.
    /// It is the only environment allowed to have primitives.
    /// It is separate from the global environment so that it can be shared between interpreter instances.
    /// </summary>
    public class PrimitiveEnvironment : Environment, IPrimitiveEnvironment
    {
        #region Constructor
        /// <summary>
        /// Initializes a new instance of the PrimitiveEnvironment class.
        /// </summary>
        private PrimitiveEnvironment() :
            base(NullInterp, Empty)
        {
            this.InstallPrimitives();
        }
        #endregion

        #region Static Methods
        /// <summary>
        /// Create a new primitive environment.
        /// This is used to share the primitives among several interpreters.
        /// </summary>
        /// <returns>The primitive environment.</returns>
        public static PrimitiveEnvironment New()
        {
            return new PrimitiveEnvironment();
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Define a primitive, taking a variable number of arguments.
        /// Creates a Primitive associated with the given name in the primitive environment.
        /// Returns the environmet.
        /// </summary>
        /// <param name="name">The primitive name.</param>
        /// <param name="operation">The operation to perform.</param>
        /// <param name="minArgs">The minimum number of arguments.</param>
        /// <param name="maxArgs">The maximum number of arguments.</param>
        /// <returns>A refernce to the environment.</returns>
        public IPrimitiveEnvironment DefinePrim(Object name, Primitive.Op operation, int minArgs, int maxArgs)
        {
            return this.DefinePrimitive(name, operation, minArgs, maxArgs);
        }

        /// <summary>
        /// Define a primitive, taking a fixed number of arguments.
        /// Creates a Primitive associated with the given name in the primitive environment.
        /// Returns the environmet.
        /// </summary>
        /// <param name="name">The primitive name.</param>
        /// <param name="operation">The operation to perform.</param>
        /// <param name="numberOfArgs">The number of arguments.</param>
        /// <returns>A refernce to the environment.</returns>
        public IPrimitiveEnvironment DefinePrim(Object name, Primitive.Op operation, int numberOfArgs)
        {
            return this.DefinePrimitive(name, operation, numberOfArgs);
        }
        #endregion

        #region Internal Methods
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
        internal PrimitiveEnvironment DefinePrimitive(Object name, Primitive.Op operation, int minArgs, int maxArgs)
        {
            this.Define(name, new Primitive(operation, minArgs, maxArgs));
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
        internal PrimitiveEnvironment DefinePrimitive(Object name, Primitive.Op operation, int numberOfArgs)
        {
            this.Define(name, new Primitive(operation, numberOfArgs, numberOfArgs));
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
                        System.Environment.Exit(EmptyList.IsType(First(args)) ? 0 : (int)Number.Num(First(args)));
                        return Undefined.Instance;
                    },
                    0,
                    1)
                .DefinePrimitive("time-call", (args, caller) => EvaluateTimeCall.Call(args, caller.Env, caller), 1, 2);
        }
        #endregion
    }
}