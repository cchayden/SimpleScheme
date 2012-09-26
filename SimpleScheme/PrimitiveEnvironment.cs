// <copyright file="PrimitiveEnvironment.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
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
        /// <param name="argTypes">The argument types.</param>
        /// <returns>A refernce to the environment.</returns>
        public IPrimitiveEnvironment DefinePrimitive(string name, Func<Obj, Evaluator, Obj> operation, int minArgs, int maxArgs, params Primitive.ValueType[] argTypes)
        {
            this.Define(Symbol.New(name), Primitive.New(operation, minArgs, maxArgs, argTypes));
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
        /// <param name="argTypes">The argument types.</param>
        /// <returns>A refernce to the environment.</returns>
        public IPrimitiveEnvironment DefinePrimitive(string name, Func<Obj, Evaluator, Obj> operation, int numberOfArgs, params Primitive.ValueType[] argTypes)
        {
            this.Define(Symbol.New(name), Primitive.New(operation, numberOfArgs, numberOfArgs, argTypes));
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
                        System.Environment.Exit(args.First().IsEmptyList() ? 0 : Number.AsInt(args.First()));
                        return Undefined.New();
                    },
                    0,
                    1, 
                    Primitive.ValueType.Number)
                .DefinePrimitive("time-call", (args, caller) => EvaluateTimeCall.Call(args, caller.Env, caller), 1, 2, Primitive.ValueType.Proc, Primitive.ValueType.Number);
        }
        #endregion
    }
}