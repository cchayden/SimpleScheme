// <copyright file="ClrConstructor.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Diagnostics.Contracts;
    using System.Reflection;

    /// <summary>
    /// Handles  CLR method constructor calls.
    /// This is used when it is necessary to create instances with non-default constructor.
    /// Immutable class.
    /// </summary>
    internal sealed class ClrConstructor : ClrProcedure
    {
        #region Fields
        /// <summary>
        /// The type of the class to construct.
        /// </summary>
        private readonly Type classType;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the ClrConstructor class.
        /// </summary>
        /// <param name="targetClassName">The class of the object to invoke.</param>
        /// <param name="argClasses">The types of each argument.</param>
        private ClrConstructor(string targetClassName, Type[] argClasses)
            : base(targetClassName, "ctor", null, null, argClasses, argClasses.Length)
        {
            Contract.Requires(targetClassName != null);
            Contract.Requires(argClasses != null);
            try
            {
                this.classType = targetClassName.ToClass();
            }
            catch (TypeLoadException)
            {
                ErrorHandlers.ClrError("Bad class, can't load: " + targetClassName);
            }

            if (this.classType == null)
            {
                ErrorHandlers.ClrError("Target type cannot be found in constructor: " + targetClassName);
            }
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Display the clr constructor as a string.  
        /// </summary>
        /// <returns>The string form of the constructor.</returns>
        public override string ToString()
        {
            return "<clr-constructor>";
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the sync clr procedure primitives.
        /// </summary>
        /// <param name="primEnv">The environment to define the primitives into.</param>
        internal static new void DefinePrimitives(PrimitiveEnvironment primEnv)
        {
            Contract.Requires(primEnv != null);
            const int MaxInt = int.MaxValue;
            primEnv
                .DefinePrimitive(
                    "constructor", 
                    new[] { "(constructor <class-name> <arg-class-name> ...)" },
                    (args, env, caller) => new ClrConstructor(First(args).ToString(), ClassList(Rest(args))),
                    new ArgsInfo(1, MaxInt, ArgType.StringOrSymbol));
        }
        #endregion

        #region Internal Methods

        /// <summary>
        /// Execute the constructor.
        /// Match all arguments supplied up to the constructor's types.
        /// </summary>
        /// <param name="args">Arguments to pass to the constructor.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <param name="returnTo">The evaluator to return to.  This can be different from caller if this is the last step in evaluation</param>
        /// <returns>The next evaluator to excute.</returns>
        internal override Evaluator Apply(SchemeObject args, Evaluator returnTo)
        {
#if Check
            this.CheckArgCount(ListLength(args), args, "ClrConstructor");
#endif
            Assembly assembly = this.classType.Assembly;
            object[] argArray = this.ToArgList(args, null, "ClrConstructor");
            object res = assembly.CreateInstance(this.classType.FullName, false, BindingFlags.Default, null, argArray, null, null);
            if (res == null)
            {
                ErrorHandlers.ClrError("Constructor failed");
                return null;
            }

            returnTo.ReturnedExpr = new ClrObject(res);
            return returnTo;
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.classType != null);
        }
        #endregion
    }
}