// <copyright file="Primitive.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
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
        /// The name of the evaluator, used for counters and tracing.
        /// </summary>
        public new const string Name = "primitive";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int Counter = SimpleScheme.Counter.Create(Name);
        #endregion

        #region Fields
        /// <summary>
        /// The code to perform the operation.
        /// the evaluator function is executed to perform the primitive operation.
        /// It takes two arguments, a caller and args.
        /// The caller is the evaluator to return to when the operation is done.
        /// The args is the operand.
        /// The return value is either
        /// (1) a value, the operation result, or
        /// (2) a Evaluator, The next evaluator to execute. 
        /// </summary>
        private readonly Func<object, Evaluator, object> operation;

        /// <summary>
        /// The argument types.
        /// Arguments are checked against these types, if they are supplied.
        /// </summary>
        private readonly ValueType[] argTypes;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Primitive class.
        /// </summary>
        /// <param name="operation">The code to carry out the operation.</param>
        /// <param name="minArgs">The minimum number of arguments.</param>
        /// <param name="maxArgs">The maximum number of arguments.</param>
        /// <param name="argTypes">The argument types.</param>
        public Primitive(Func<object, Evaluator, object> operation, int minArgs, int maxArgs, ValueType[] argTypes) :
            base(minArgs, maxArgs)
        {
            this.operation = operation;
            this.argTypes = argTypes;
            if (maxArgs > 0 && argTypes.Length == 0)
            {
                throw new ErrorHandlers.SchemeException("Invalid primitive: " + operation);
            }
        }
        #endregion

        #region Enums
        /// <summary>
        /// This enum contains all the types that values can have.  It also contains
        /// a few combination types that are used in checking arguments to
        /// primitives.
        /// </summary>
        public enum ValueType
        {
            /// <summary>
            /// Any object is required.
            /// </summary>
            Obj,

            /// <summary>
            /// A pair is required..
            /// </summary>
            Pair,

            /// <summary>
            /// A pair or the empty object is required.
            /// </summary>
            PairOrEmpty,

            /// <summary>
            /// A pair or a symbol is required.
            /// </summary>
            PairOrSymbol,

            /// <summary>
            /// A number is required.
            /// </summary>
            Number,

            /// <summary>
            /// A character is required.
            /// </summary>
            Char,

            /// <summary>
            /// A string is required.
            /// </summary>
            String,

            /// <summary>
            /// A procedure is required.  This includes macros and primitives, as well
            /// as lambdas.
            /// </summary>
            Proc,

            /// <summary>
            /// A vector is required.
            /// </summary>
            Vector,

            /// <summary>
            /// A symbol is required.
            /// </summary>
            Symbol,

            /// <summary>
            /// A boolean is required.
            /// </summary>
            Boolean,

            /// <summary>
            /// A port is required.
            /// </summary>
            Port,

            //// The following are not used as primitive arguments.

            /// <summary>
            /// The empty list.
            /// </summary>
            Empty,

            /// <summary>
            /// An asynchronous clr procedure.
            /// </summary>
            AsynchronousClrProcedure,

            /// <summary>
            /// A synchronous clr procedure.
            /// </summary>
            SynchronousClrProcedure,

            /// <summary>
            /// A CLR constructor.
            /// </summary>
            ClrConstructor,

            /// <summary>
            /// A continuation.
            /// </summary>
            Continuation,

            /// <summary>
            /// A lambda.
            /// </summary>
            Lambda,

            /// <summary>
            /// A macro
            /// </summary>
            Macro,

            /// <summary>
            /// The undefined object.
            /// </summary>
            Undefined
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write the primitive to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public override void AsString(bool quoted, StringBuilder buf)
        {
            buf.Append(this.ToString());
        }

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
        ///   a result or a Evaluator.  If they return an evaluator, it means the result
        ///   is not yet ready, and that a new evaluator was created and returned.  When
        ///   that evaluator has a result, it will put in into ReturnedResult and return to the
        ///   caller evaluator provided to it.
        /// If there is a result available immediately, this returns it by storing it
        ///   in ReturnedResult and returning to the caller.
        /// </summary>
        /// <param name="args">The arguments to the primitive.</param>
        /// <param name="caller">The calling Evaluator.</param>
        /// <returns>The next evaluator to execute.</returns>
        public override Evaluator Apply(Obj args, Evaluator caller)
        {
            // First check the number of arguments
            this.CheckArgs(args, "Primitive");
            this.CheckArgTypes(args);
            caller.IncrementCounter(Counter);

            // Execute the operation
            var res = this.operation(args, caller);

            // See if the operation returns a result or another evaluator
            var evaluator = res as Evaluator;
            if (evaluator != null)
            {
                return evaluator;
            }

            // Operation returned a result -- just return this
            //  to the caller.
            return caller.UpdateReturnValue(res);
        }
        #endregion

        /// <summary>
        /// Check the argument types
        /// </summary>
        /// <param name="args">The arguments passed to the primitive.</param>
        private void CheckArgTypes(Obj args)
        {
            int numArgs = List.Length(args);
            int numTypes = this.argTypes.Length - 1;
            for (int i = 0; i < numArgs; i++)
            {
                if (args == null)
                {
                    return;
                }

                int t = i < numTypes ? i : numTypes;
                this.CheckArgType(List.First(args), this.argTypes[t]);
                args = List.Rest(args);
            }
        }

        /// <summary>
        /// Check one of the arguments
        /// </summary>
        /// <param name="arg">An argument passed to the primitive.</param>
        /// <param name="argType">The expected argument type.</param>
        private void CheckArgType(Obj arg, ValueType argType)
        {
            switch (argType)
            {
                case ValueType.Obj:
                    return;
                case ValueType.Pair:
                    if (Pair.Is(arg))
                    {
                        return;
                    }

                    break;
                case ValueType.PairOrEmpty:
                    if (Pair.Is(arg) || EmptyList.Is(arg))
                    {
                        return;
                    }

                    break;
                case ValueType.PairOrSymbol:
                    if (Pair.Is(arg) || Symbol.Is(arg))
                    {
                        return;
                    }

                    break;
                case ValueType.Number:
                    if (Number.Is(arg))
                    {
                        return;
                    }

                    break;
                case ValueType.Char:
                    if (Character.Is(arg))
                    {
                        return;
                    }

                    break;
                case ValueType.String:
                    if (SchemeString.Is(arg)) 
                    {
                        return;
                    }

                    break;
                case ValueType.Proc:
                    if (Is(arg))
                    {
                        return;
                    }

                    break;
                case ValueType.Vector:
                    if (Vector.Is(arg))
                    {
                        return;
                    }

                    break;
                case ValueType.Boolean:
                    if (SchemeBoolean.Is(arg))
                    {
                        return;
                    }

                    break;
                case ValueType.Symbol:
                    if (Symbol.Is(arg))
                    {
                        return;
                    }

                    break;
                case ValueType.Port:
                    if (InputPort.Is(arg) || OutputPort.Is(arg))
                    {
                        return;
                    }

                    break;
            }

            var msg = string.Format(
                "Primitive {0} invalid argument {1} type: {2} expected: {3}", 
                this.ProcedureName, 
                Printer.AsString(arg),
                Printer.TypeName(arg), 
                argType.ToString());
            ErrorHandlers.SemanticError(msg);
        }
    }
}