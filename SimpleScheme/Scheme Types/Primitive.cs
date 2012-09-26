﻿// <copyright file="Primitive.cs" company="Charles Hayden">
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
        private static readonly int counter = Counter.Create(Name);
        #endregion

        #region Static fields
        /// <summary>
        /// Tests arguments to see if they have the expected type.
        /// </summary>
        private static readonly ArgTypeTester tester = new ArgTypeTester();
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
        private Primitive(Func<object, Evaluator, object> operation, int minArgs, int maxArgs, ValueType[] argTypes) :
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

        #region Public Static Methods
        /// <summary>
        /// Initializes a new instance of the Primitive class.
        /// </summary>
        /// <param name="operation">The code to carry out the operation.</param>
        /// <param name="minArgs">The minimum number of arguments.</param>
        /// <param name="maxArgs">The maximum number of arguments.</param>
        /// <param name="argTypes">The argument types.</param>
        /// <returns>A new primitive.</returns>
        public static Primitive New(Func<object, Evaluator, object> operation, int minArgs, int maxArgs, ValueType[] argTypes)
        {
            return new Primitive(operation, minArgs, maxArgs, argTypes);
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
        public new void PrintString(bool quoted, StringBuilder buf)
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
            int numArgs = this.CheckArgs(args, "Primitive");
            this.CheckArgTypes(numArgs, args);
            caller.IncrementCounter(counter);

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

        #region Private Methods
        /// <summary>
        /// Check the argument types
        /// </summary>
        /// <param name="numArgs">The number of arguments passed to the procedure.</param>
        /// <param name="args">The arguments passed to the primitive.</param>
        private void CheckArgTypes(int numArgs, Obj args)
        {
            int numTypes = this.argTypes.Length - 1;
            for (int i = 0; i < numArgs; i++)
            {
                if (args == null)
                {
                    return;
                }

                int t = i < numTypes ? i : numTypes;
                this.CheckArgType(args.First(), this.argTypes[t]);
                args = args.Rest();
            }
        }

        /// <summary>
        /// Check one of the arguments
        /// </summary>
        /// <param name="arg">An argument passed to the primitive.</param>
        /// <param name="argType">The expected argument type.</param>
        private void CheckArgType(Obj arg, ValueType argType)
        {
            if (tester.Ok(arg, argType))
            {
                return;
            }

            var msg = string.Format(
                "Primitive {0} invalid argument {1} type: {2} expected: {3}", 
                this.ProcedureName, 
                Printer.AsString(arg),
                Printer.TypeName(arg), 
                argType);
            ErrorHandlers.SemanticError(msg);
        }
        #endregion

        #region Private Class
        /// <summary>
        /// Tests arguments to see if they have the expected type.
        /// </summary>
        private class ArgTypeTester
        {
            /// <summary>
            /// Contains argument type predicates, indexed by argument type specifier.
            /// </summary>
            private Predicate<object>[] argPredicates;

            /// <summary>
            /// Initializes the argument type list.
            /// </summary>
            public ArgTypeTester()
            {
                argPredicates = new Predicate<object>[(int)ValueType.Undefined];
                this.Set(ValueType.Obj, arg => true);
                this.Set(ValueType.Pair, Pair.Is);
                this.Set(ValueType.PairOrEmpty, arg => Pair.Is(arg) || EmptyList.Is(arg));
                this.Set(ValueType.PairOrSymbol, arg => Pair.Is(arg) || Symbol.Is(arg));
                this.Set(ValueType.Number, Number.Is);
                this.Set(ValueType.Char, Character.Is);
                this.Set(ValueType.String, SchemeString.Is);
                this.Set(ValueType.Proc, Procedure.Is);
                this.Set(ValueType.Vector, Vector.Is);
                this.Set(ValueType.Boolean, SchemeBoolean.Is);
                this.Set(ValueType.Symbol, Symbol.Is);
                this.Set(ValueType.Port, (arg) => InputPort.Is(arg) || OutputPort.Is(arg));
            }

            /// <summary>
            /// Tests the argument against the expected argument type.
            /// </summary>
            /// <param name="arg">The actual argument.</param>
            /// <param name="argType">The expected argument type.</param>
            /// <returns>True if OK.</returns>
            public bool Ok(object arg, ValueType argType)
            {
                return argType <= ValueType.Undefined && this.argPredicates[(int)argType](arg);
            }

            /// <summary>
            /// Add a predicate to the arg predicates.
            /// </summary>
            /// <param name="t">The value type.</param>
            /// <param name="pred">The predicate.</param>
            private void Set(ValueType t, Predicate<object> pred)
            {
                this.argPredicates[(int)t] = pred;
            }
        }
        #endregion
    }
}