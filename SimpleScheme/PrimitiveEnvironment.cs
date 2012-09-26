// <copyright file="PrimitiveEnvironment.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics.Contracts;
    using System.Reflection;

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
        #region Fields

        /// <summary>
        /// These classes define DefinePrimitives that needs to be called to install primitives.
        /// </summary>
        private static readonly Type[] primitiveTypes = new[]
            {
                typeof(PrimitiveEnvironment), 
                typeof(EvaluateExpression), 
                typeof(Number),
                typeof(Procedure),
                typeof(List),
                typeof(InputPort),
                typeof(OutputPort),
                typeof(Vector),
                typeof(SchemeBoolean),
                typeof(SchemeString),
                typeof(Character),
                typeof(Symbol),
                typeof(ClrProcedure),
                typeof(ClrConstructor),
                typeof(SynchronousClrProcedure),
                typeof(AsynchronousClrProcedure),
                typeof(Counter),
                typeof(Debugging),
                typeof(ErrorHandlers)
            };

        /// <summary>
        /// A list of static methods to be called to initialize primitives.
        /// </summary>
        private static readonly List<MethodInfo> primitiveInitializers = new List<MethodInfo>();
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes static members of the <see cref="PrimitiveEnvironment"/> class. 
        /// Set up the primitive initializers.  The MethodInfo is looked up and stored, to
        ///   avoid having to do it each time a new primitive environment is created.
        /// </summary>
        static PrimitiveEnvironment()
        {
            foreach (Type t in primitiveTypes)
            {
                primitiveInitializers.Add(t.GetMethod("DefinePrimitives", BindingFlags.Static | BindingFlags.NonPublic, null,  new[] { typeof(PrimitiveEnvironment) }, null));
            }
        }

        /// <summary>
        /// Initializes a new instance of the PrimitiveEnvironment class.
        /// </summary>
        internal PrimitiveEnvironment()
        {
            // call DefinePrimitives in all classes that define primitives
            foreach (var mi in primitiveInitializers)
            {
                mi.Invoke(null, new object[] { this });
            }
        }
        #endregion

        #region New
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
        /// <param name="description">An array of strings describing the primitive.</param>
        /// <param name="operation">The operation to perform.</param>
        /// <param name="argsInfo">Information about primitive args.</param>
        /// <returns>A refernce to the environment.</returns>
        public IPrimitiveEnvironment DefinePrimitive(Symbol name, string[] description, Operation operation, ArgsInfo argsInfo)
        {
            if (name == null)
            {
                ErrorHandlers.ArgumentError("DefinePrimitives: name must not be null");
                return null;
            }

            if (operation == null)
            {
                ErrorHandlers.ArgumentError("DefinePrimitives: operation must not be null");
                return null;
            }

            this.Define(name, new Primitive(name.SymbolName, operation, description, argsInfo));
            return this;
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the counter primitives.
        /// </summary>
        /// <param name="primEnv">The environment to define the primitives into.</param>
        internal static void DefinePrimitives(PrimitiveEnvironment primEnv)
        {
            Contract.Requires(primEnv != null);
            primEnv
                .DefinePrimitive(
                    "exit",
                    new[] { "(exit)" },
                    (args, env, caller) => Exit(args),
                    new ArgsInfo(0, 1, ArgType.Number))
                .DefinePrimitive(
                    "time-call",
                    new[] { "(time-call <thunk>)", "(time-call <thunk> <count>)" },
                    (args, env, caller) => EvaluateTimeCall.Call((Procedure)List.First(args), List.Second(args), caller.Env, caller),
                    new ArgsInfo(1, 2, ArgType.Proc, ArgType.Number));
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Create a list of the primitives in the environment.
        /// </summary>
        /// <returns>A list of pairs containing the primitive name and value.</returns>
        internal SchemeObject ListPrimitives()
        {
            return this.ListEnv();
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Exit the whole process.
        /// </summary>
        /// <param name="args">If given, the process exit code.</param>
        /// <returns>Does not return.</returns>
        private static SchemeObject Exit(SchemeObject args)
        {
            Contract.Requires(args != null);
            System.Environment.Exit(List.First(args) is EmptyList ? 0 : Number.AsInt(List.First(args)));
            return Undefined.Instance;
        }
        #endregion
    }
}