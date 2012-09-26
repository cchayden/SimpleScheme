// <copyright file="Primitive.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;
    using System.Text;

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
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("procedure");
        #endregion

        #region Static fields
        /// <summary>
        /// Tests arguments to see if they have the expected type.
        /// </summary>
        private static readonly ArgTypeTester tester = new ArgTypeTester();

        /// <summary>
        /// Maps the argument type checking values to a message.
        /// </summary>
        private static readonly Dictionary<ArgType, string> valueMessage;
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
        private readonly Operation operation;

        /// <summary>
        /// The argument types.
        /// Arguments are checked against these types, if they are supplied.
        /// </summary>
        private readonly ArgType[] argTypes;

        /// <summary>
        /// The primitive description.
        /// </summary>
        private readonly string[] description;
        #endregion

        #region Constructors

        /// <summary>
        /// Initializes static members of the <see cref="Primitive"/> class.
        /// </summary>
        static Primitive()
        {
            valueMessage = new Dictionary<ArgType, string>
                {
                    { ArgType.Obj, "Object" },
                    { ArgType.Pair, "Pair" },
                    { ArgType.PairOrEmpty, "Pair or EmptyList" },
                    { ArgType.PairOrSymbol, "Pair or Symbol" },
                    { ArgType.Number, "Number" },
                    { ArgType.Char, "Character" },
                    { ArgType.CharOrEmpty, "Character or EmptyList" },
                    { ArgType.String, "SchemeString" },
                    { ArgType.StringOrSymbol, "SchemeString or Symbol" },
                    { ArgType.Proc, "Procedure" },
                    { ArgType.Vector, "Vector" },
                    { ArgType.Symbol, "Symbol" },
                    { ArgType.Boolean, "SchemeBoolean" },
                    { ArgType.InputPort, "InputPort" },
                    { ArgType.OutputPort, "OutputPort" },
                    { ArgType.EmptyList, "EmptyList" },
                    { ArgType.AsynchronousClrProcedure, "AsynchronousClrProcedure" },
                    { ArgType.SynchronousClrProcedure, "SynchronousClrProcedure" },
                    { ArgType.ClrConstructor, "ClrConstructor" },
                    { ArgType.ClrObject, "ClrObject" },
                    { ArgType.Continuation, "Continuation" },
                    { ArgType.Lambda, "Lambda" },
                    { ArgType.Macro, "Macro" },
                    { ArgType.Eof, "Eof" },
                    { ArgType.Undefined, "Undefined" }
                };
        }

        /// <summary>
        /// Initializes a new instance of the Primitive class.
        /// </summary>
        /// <param name="operation">The code to carry out the operation.</param>
        /// <param name="description">The primitive description.</param>
        /// <param name="minArgs">The minimum number of arguments.</param>
        /// <param name="maxArgs">The maximum number of arguments.</param>
        /// <param name="argTypes">The argument types.</param>
        public Primitive(Operation operation, string[] description, int minArgs, int maxArgs, ArgType[] argTypes) :
            base(minArgs, maxArgs)
        {
            this.operation = operation;
            this.description = description;
            this.argTypes = argTypes;
            if (maxArgs > 0 && argTypes.Length == 0)
            {
                throw new ErrorHandlers.SchemeException(string.Format(@"Invalid primitive ""{0}""", operation));
            }
        }
        #endregion

        /// <summary>
        /// This enum contains all the types that values can have.  It also contains
        /// a few combination types that are used in checking arguments to
        /// primitives.
        /// </summary>
        public enum ArgType
        {
            /// <summary>
            /// Any object is required.
            /// Must be first.
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
            /// A character or the empty object is required.
            /// </summary>
            CharOrEmpty,

            /// <summary>
            /// A string is required.
            /// </summary>
            String,

            /// <summary>
            /// A string or symbol is required.
            /// </summary>
            StringOrSymbol,

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
            /// An input port is required.
            /// </summary>
            InputPort,

            /// <summary>
            /// An output port is required.
            /// </summary>
            OutputPort,

            //// The following are not used as primitive arguments.

            /// <summary>
            /// The empty list.
            /// </summary>
            EmptyList,

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
            /// A CLR object, created by a constructor
            /// </summary>
            ClrObject,

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
            /// Eof object
            /// </summary>
            Eof,

            /// <summary>
            /// The undefined object.
            /// Must be last.
            /// </summary>
            Undefined
        }

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
        /// Describe the primitive by returning its description fields.
        /// </summary>
        /// <returns>The primitive description.</returns>
        public override string Describe()
        {
            var sb = new StringBuilder();
            Array.ForEach(this.description, str => sb.AppendLine(str));
            return sb.ToString();
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
        public override Evaluator Apply(SchemeObject args, Evaluator caller)
        {
            // First check the number of arguments
#if Check
            int numArgs = this.CheckArgs(args, typeof(Primitive));
            this.CheckArgTypes(numArgs, args);
#endif
#if Diagnostics
            caller.IncrementCounter(counter);
#endif

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
            return caller.UpdateReturnValue((SchemeObject)res);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Check the argument types
        /// </summary>
        /// <param name="numArgs">The number of arguments passed to the procedure.</param>
        /// <param name="args">The arguments passed to the primitive.</param>
        private void CheckArgTypes(int numArgs, SchemeObject args)
        {
            int numTypes = this.argTypes.Length - 1;
            for (int i = 0; i < numArgs; i++)
            {
                if (args == null)
                {
                    return;
                }

                int t = i < numTypes ? i : numTypes;
                this.CheckArgType(First(args), this.argTypes[t]);
                args = Rest(args);
            }
        }

        /// <summary>
        /// Check one of the arguments
        /// </summary>
        /// <param name="arg">An argument passed to the primitive.</param>
        /// <param name="argType">The expected argument type.</param>
        private void CheckArgType(SchemeObject arg, ArgType argType)
        {
            if (tester.Ok(arg, argType))
            {
                return;
            }

            var msg = string.Format(
                @"Primitive ""{0}"" invalid argument ""{1}"". Got type: {2}. Expected type: {3}", 
                this.ProcedureName, 
                arg.ToString(true),
                arg.GetType().Name,
                valueMessage[argType]);
            ErrorHandlers.SemanticError(msg, arg);
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
            private readonly Predicate<SchemeObject>[] argPredicates;

            /// <summary>
            /// Initializes a new instance of the ArgTypeTester class.
            /// </summary>
            public ArgTypeTester()
            {
                this.argPredicates = new Predicate<SchemeObject>[(int)ArgType.Undefined];
                this.Set(ArgType.Obj, arg => true);
                this.Set(ArgType.Pair, arg => arg is Pair);
                this.Set(ArgType.PairOrEmpty, arg => arg is Pair || arg is EmptyList);
                this.Set(ArgType.PairOrSymbol, arg => arg is Pair || arg is Symbol);
                this.Set(ArgType.Number, arg => arg is Number);
                this.Set(ArgType.Char, arg => arg is Character);
                this.Set(ArgType.CharOrEmpty, arg => arg is Character || arg is EmptyList);
                this.Set(ArgType.String, arg => arg is SchemeString);
                this.Set(ArgType.StringOrSymbol, arg => arg is SchemeString || arg is Symbol);
                this.Set(ArgType.Proc, arg => arg is Procedure);
                this.Set(ArgType.Vector, arg => arg is Vector);
                this.Set(ArgType.Boolean, arg => arg is SchemeBoolean);
                this.Set(ArgType.Symbol, arg => arg is Symbol);
                this.Set(ArgType.InputPort, arg => arg is InputPort);
                this.Set(ArgType.OutputPort, arg => arg is OutputPort);
            }

            /// <summary>
            /// Tests the argument against the expected argument type.
            /// </summary>
            /// <param name="arg">The actual argument.</param>
            /// <param name="argType">The expected argument type.</param>
            /// <returns>True if OK.</returns>
            public bool Ok(SchemeObject arg, ArgType argType)
            {
                return argType <= ArgType.Undefined && this.argPredicates[(int)argType](arg);
            }

            /// <summary>
            /// Add a predicate to the arg predicates.
            /// </summary>
            /// <param name="t">The value type.</param>
            /// <param name="pred">The predicate.</param>
            private void Set(ArgType t, Predicate<SchemeObject> pred)
            {
                this.argPredicates[(int)t] = pred;
            }
        }
        #endregion
    }
}