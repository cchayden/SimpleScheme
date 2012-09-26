// <copyright file="Primitive.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Diagnostics.Contracts;
    using System.Text;

    #region Enum
    /// <summary>
    /// This enum contains all the types that values can have.  It also contains
    /// a few combination types that are used in checking arguments to
    /// primitives.
    /// This is not a bit vector (dedined as [Flags]) because we want to use the values
    ///   as an array index to get the validation predicate and error message.
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
        /// An input port is required.
        /// </summary>
        InputPort,

        /// <summary>
        /// An output port is required.
        /// </summary>
        OutputPort,

        //// The next are combinations of types

        /// <summary>
        /// A pair or the empty object is required.
        /// </summary>
        PairOrEmpty,

        /// <summary>
        /// A pair or a symbol is required.
        /// </summary>
        PairOrSymbol,

        /// <summary>
        /// A pair or a symbol or an empty list is required.
        /// </summary>
        PairOrSymbolOrEmpty,

        /// <summary>
        /// A character or the empty object is required.
        /// </summary>
        CharOrEmpty,

        /// <summary>
        /// A string or symbol is required.
        /// </summary>
        StringOrSymbol,

        //// The rest are not used as primitive arguments.

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
    #endregion

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
        /// Initializes a new instance of the Primitive class.
        /// </summary>
        /// <param name="name">The primitive name.</param>
        /// <param name="operation">The code to carry out the operation.</param>
        /// <param name="description">The primitive description.</param>
        /// <param name="argsInfo">Information about primitive args.</param>
        public Primitive(string name, Operation operation, string[] description, ArgsInfo argsInfo) :
            base(name, argsInfo)
        {
            Contract.Requires(name != null);
            Contract.Requires(operation != null);
            Contract.Requires(description != null);
            this.operation = operation;
            this.description = description;
            this.argTypes = argsInfo.ArgTypes;
            if (argsInfo.MaxArgs > 0 && argTypes.Length == 0)
            {
                throw new ErrorHandlers.SchemeException(string.Format(@"Invalid primitive definition ""{0}""", operation));
            }
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
        ///   a result or a Evaluator.  If they return an evaluator, it means the result
        ///   is not yet ready, and that a new evaluator was created and returned.  When
        ///   that evaluator has a result, it will put in into ReturnedResult and return to the
        ///   caller evaluator provided to it.
        /// If there is a result available immediately, this returns it by storing it
        ///   in ReturnedResult and returning to the caller.
        /// </summary>
        /// <param name="args">The arguments to the primitive.</param>
        /// <param name="returnTo">The evaluator to return to.  This can be different from caller if this is the last step in evaluation</param>
        /// <param name="caller">The calling Evaluator.</param>
        /// <returns>The next evaluator to execute.</returns>
        internal override Evaluator Apply(SchemeObject args, Evaluator returnTo, Evaluator caller)
        {
            return this.Apply(args, null, returnTo, caller);
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
        /// <param name="env">The environment in which the application should take place.</param>
        /// <param name="returnTo">The evaluator to return to.  This can be different from caller if this is the last step in evaluation</param>
        /// <param name="caller">The calling Evaluator.</param>
        /// <returns>The next evaluator to execute.</returns>
        internal Evaluator Apply(SchemeObject args, Environment env, Evaluator returnTo, Evaluator caller)
        {
            Contract.Requires(args != null);
            //// env may be null (mostly it is, meaning the primitive usually does not depend on the environment)
            Contract.Requires(returnTo != null);
            Contract.Requires(caller != null);

            // First check the number of arguments
#if Check
            this.CheckArgTypes(args, caller);
#endif
#if Diagnostics
            returnTo.IncrementCounter(counter);
#endif

            // Execute the operation
            var res = this.operation(args, env, returnTo);

            // See if the operation returns a result or another evaluator
            if (res is Evaluator)
            {
                return (Evaluator)res;
            }

            // Operation returned a result -- just return this
            //  to the caller.
            Contract.Assume(res is SchemeObject);   // it's not an Evaluator; it must be SchemeObject
            returnTo.ReturnedExpr = (SchemeObject)res;
            return returnTo;
        }

        #endregion

        #region Internal Methods
        /// <summary>
        /// Describe the primitive by returning its description fields.
        /// </summary>
        /// <returns>The primitive description.</returns>
        internal override string Describe()
        {
            var sb = new StringBuilder();
            Array.ForEach(this.description, str => sb.AppendLine(str));
            return sb.ToString();
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Check the argument types
        /// </summary>
        /// <param name="args">The arguments passed to the primitive.</param>
        /// <param name="caller">The calling evaluator</param>
        private void CheckArgTypes(SchemeObject args, Evaluator caller)
        {
            Contract.Requires(args != null);
            Contract.Requires(caller != null);
            int numArgs = ListLength(args);
            this.CheckArgCount(numArgs, args, this.ProcedureName, caller);
            int numTypes = this.argTypes.Length - 1;
            for (int i = 0; i < numArgs; i++)
            {
                if (args == null)
                {
                    return;
                }

                int t = i < numTypes ? i : numTypes;
                Contract.Assume(t >= 0);
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
            Contract.Requires(arg != null);
            if (ArgTypeTester.Ok(arg, argType))
            {
                return;
            }

            var msg = string.Format(
                @"Primitive ""{0}"" invalid argument ""{1}"". Got type: {2}. Expected type: {3}", 
                this.ProcedureName, 
                arg.ToString(true),
                arg.GetType().Name,
                ArgTypeTester.ValueMessage(argType));
            ErrorHandlers.SemanticError(msg, arg);
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.operation != null);
            Contract.Invariant(this.description != null);
            Contract.Invariant(this.argTypes != null);
            Contract.Invariant(this.argTypes != null);
        }
        #endregion

        #region Private Class
        /// <summary>
        /// Tests arguments to see if they have the expected type.
        /// </summary>
        private static class ArgTypeTester
        {
            /// <summary>
            /// Contains argument type predicates, indexed by argument type specifier.
            /// </summary>
            private static readonly Predicate<SchemeObject>[] argPredicates;

            /// <summary>
            /// Maps the argument type checking values to a message.
            /// </summary>
            private static readonly string[] valueMessage;

            /// <summary>
            /// Initializes static members of the <see cref="ArgTypeTester"/> class.
            /// </summary>
            static ArgTypeTester()
            {
                Contract.Ensures(Contract.ForAll(0, Primitive.ArgTypeTester.valueMessage.Length, idx => Primitive.ArgTypeTester.argPredicates[idx] != null));
                Contract.Ensures(Contract.ForAll(0, Primitive.ArgTypeTester.valueMessage.Length, idx => Primitive.ArgTypeTester.valueMessage[idx] != null));
                argPredicates = new Predicate<SchemeObject>[(int)ArgType.Undefined + 1];
                argPredicates[(int)ArgType.Obj] = arg => true;
                argPredicates[(int)ArgType.Pair] = arg => arg is Pair;
                argPredicates[(int)ArgType.Number] = arg => arg is Number;
                argPredicates[(int)ArgType.Char] = arg => arg is Character;
                argPredicates[(int)ArgType.String] = arg => arg is SchemeString;
                argPredicates[(int)ArgType.Proc] = arg => arg is Procedure;
                argPredicates[(int)ArgType.Vector] = arg => arg is Vector;
                argPredicates[(int)ArgType.Boolean] = arg => arg is SchemeBoolean;
                argPredicates[(int)ArgType.Symbol] = arg => arg is Symbol;
                argPredicates[(int)ArgType.InputPort] = arg => arg is InputPort;
                argPredicates[(int)ArgType.OutputPort] = arg => arg is OutputPort;
                //// combinations -- arguments can be one of several types.
                argPredicates[(int)ArgType.PairOrEmpty] = arg => arg is Pair || arg is EmptyList;
                argPredicates[(int)ArgType.PairOrSymbol] = arg => arg is Pair || arg is Symbol;
                argPredicates[(int)ArgType.PairOrSymbolOrEmpty] = arg => arg is Pair || arg is Symbol || arg is EmptyList;
                argPredicates[(int)ArgType.CharOrEmpty] = arg => arg is Character || arg is EmptyList;
                argPredicates[(int)ArgType.StringOrSymbol] = arg => arg is SchemeString || arg is Symbol;
                //// the rest are not actually used, meaning no primitive requires an argument of these types
                argPredicates[(int)ArgType.EmptyList] = arg => arg is EmptyList;
                argPredicates[(int)ArgType.AsynchronousClrProcedure] = arg => arg is AsynchronousClrProcedure;
                argPredicates[(int)ArgType.SynchronousClrProcedure] = arg => arg is SynchronousClrProcedure;
                argPredicates[(int)ArgType.ClrConstructor] = arg => arg is ClrConstructor;
                argPredicates[(int)ArgType.ClrObject] = arg => arg is ClrObject;
                argPredicates[(int)ArgType.Continuation] = arg => arg is Continuation;
                argPredicates[(int)ArgType.Lambda] = arg => arg is Lambda;
                argPredicates[(int)ArgType.Macro] = arg => arg is Macro;
                argPredicates[(int)ArgType.Eof] = arg => arg is Eof;
                argPredicates[(int)ArgType.Undefined] = arg => arg is Undefined;

                valueMessage = new string[(int)ArgType.Undefined + 1];
                valueMessage[(int)ArgType.Obj] = "Object";
                valueMessage[(int)ArgType.Pair] = "Pair";
                valueMessage[(int)ArgType.Number] = "Number";
                valueMessage[(int)ArgType.Char] = "Character";
                valueMessage[(int)ArgType.String] = "SchemeString";
                valueMessage[(int)ArgType.Proc] = "Procedure";
                valueMessage[(int)ArgType.Vector] = "Vector";
                valueMessage[(int)ArgType.Symbol] = "Symbol";
                valueMessage[(int)ArgType.Boolean] = "SchemeBoolean";
                valueMessage[(int)ArgType.InputPort] = "InputPort";
                valueMessage[(int)ArgType.OutputPort] = "OutputPort";

                valueMessage[(int)ArgType.PairOrEmpty] = "Pair or EmptyList";
                valueMessage[(int)ArgType.PairOrSymbol] = "Pair or Symbol";
                valueMessage[(int)ArgType.PairOrSymbolOrEmpty] = "Pair or Symbol or EmptyList";
                valueMessage[(int)ArgType.CharOrEmpty] = "Character or EmptyList";
                valueMessage[(int)ArgType.StringOrSymbol] = "SchemeString or Symbol";

                valueMessage[(int)ArgType.EmptyList] = "EmptyList";
                valueMessage[(int)ArgType.AsynchronousClrProcedure] = "AsynchronousClrProcedure";
                valueMessage[(int)ArgType.SynchronousClrProcedure] = "SynchronousClrProcedure";
                valueMessage[(int)ArgType.ClrConstructor] = "ClrConstructor";
                valueMessage[(int)ArgType.ClrObject] = "ClrObject";
                valueMessage[(int)ArgType.Continuation] = "Continuation";
                valueMessage[(int)ArgType.Lambda] = "Lambda";
                valueMessage[(int)ArgType.Macro] = "Macro";
                valueMessage[(int)ArgType.Eof] = "Eof";
                valueMessage[(int)ArgType.Undefined] = "Undefined";
            }

            /// <summary>
            /// Gets the string associated with the expected argument type. 
            /// </summary>
            /// <param name="argType">The argument type.</param>
            /// <returns>The string describing the argument type.</returns>
            internal static string ValueMessage(ArgType argType)
            {
                return valueMessage[(int)argType];
            }

            /// <summary>
            /// Tests the argument against the expected argument type.
            /// </summary>
            /// <param name="arg">The actual argument.</param>
            /// <param name="argType">The expected argument type.</param>
            /// <returns>True if OK.</returns>
            internal static bool Ok(SchemeObject arg, ArgType argType)
            {
                return argType <= ArgType.Undefined && argPredicates[(int)argType](arg);
            }
        }
        #endregion
    }
}