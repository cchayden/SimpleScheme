// <copyright file="Primitive.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
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
        private readonly Func<SchemeObject, Evaluator, SchemeObject> operation;

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
        public Primitive(Func<SchemeObject, Evaluator, SchemeObject> operation, int minArgs, int maxArgs, ValueType[] argTypes) :
            base(minArgs, maxArgs)
        {
            this.operation = operation;
            this.argTypes = argTypes;
            if (maxArgs > 0 && argTypes.Length == 0)
            {
                throw new ErrorHandlers.SchemeException(string.Format(@"Invalid primitive ""{0}""", operation));
            }
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
        public override Evaluator Apply(SchemeObject args, Evaluator caller)
        {
            // First check the number of arguments
            int numArgs = this.CheckArgs(args, typeof(Primitive));
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
        private void CheckArgType(SchemeObject arg, ValueType argType)
        {
            if (tester.Ok(arg, argType))
            {
                return;
            }

            var msg = string.Format(
                @"Primitive ""{0}"" invalid argument ""{1}"" type: {2} expected: {3}", 
                this.ProcedureName, 
                arg.ToString(true),
                arg.TypeName,
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
            private readonly Predicate<SchemeObject>[] argPredicates;

            /// <summary>
            /// Initializes a new instance of the ArgTypeTester class.
            /// </summary>
            public ArgTypeTester()
            {
                this.argPredicates = new Predicate<SchemeObject>[(int)ValueType.Undefined];
                this.Set(ValueType.Obj, arg => true);
                this.Set(ValueType.Pair, arg => arg is Pair);
                this.Set(ValueType.PairOrEmpty, arg => arg is Pair || arg is EmptyList);
                this.Set(ValueType.PairOrSymbol, arg => arg is Pair || arg is Symbol);
                this.Set(ValueType.Number, arg => arg is Number);
                this.Set(ValueType.Char, arg => arg is Character);
                this.Set(ValueType.CharOrEmpty, arg => arg is Character || arg is EmptyList);
                this.Set(ValueType.String, arg => arg is SchemeString);
                this.Set(ValueType.StringOrSymbol, arg => arg is SchemeString || arg is Symbol);
                this.Set(ValueType.Proc, arg => arg is Procedure);
                this.Set(ValueType.Vector, arg => arg is Vector);
                this.Set(ValueType.Boolean, arg => arg is SchemeBoolean);
                this.Set(ValueType.Symbol, arg => arg is Symbol);
                this.Set(ValueType.InputPort, arg => arg is InputPort);
                this.Set(ValueType.OutputPort, arg => arg is OutputPort);
            }

            /// <summary>
            /// Tests the argument against the expected argument type.
            /// </summary>
            /// <param name="arg">The actual argument.</param>
            /// <param name="argType">The expected argument type.</param>
            /// <returns>True if OK.</returns>
            public bool Ok(SchemeObject arg, ValueType argType)
            {
                return argType <= ValueType.Undefined && this.argPredicates[(int)argType](arg);
            }

            /// <summary>
            /// Add a predicate to the arg predicates.
            /// </summary>
            /// <param name="t">The value type.</param>
            /// <param name="pred">The predicate.</param>
            private void Set(ValueType t, Predicate<SchemeObject> pred)
            {
                this.argPredicates[(int)t] = pred;
            }
        }
        #endregion
    }
}