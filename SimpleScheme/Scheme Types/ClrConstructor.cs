// <copyright file="ClrConstructor.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
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

        #region Define Primitives
        /// <summary>
        /// Define the sync clr procedure primitives.
        /// </summary>
        /// <param name="primEnv">The environment to define the primitives into.</param>
        internal static new void DefinePrimitives(PrimitiveEnvironment primEnv)
        {
            const int MaxInt = int.MaxValue;
            primEnv
                .DefinePrimitive(
                    "constructor", 
                    new[] { "(constructor <class-name> <arg-class-name> ...)" },
                    (args, env, caller) => new ClrConstructor(First(args).ToString(), ClassList(Rest(args))),
                    new ArgsInfo(1, MaxInt, ArgType.StringOrSymbol));
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

        /// <summary>
        /// Execute the constructor.
        /// Match all arguments supplied up to the constructor's types.
        /// </summary>
        /// <param name="args">Arguments to pass to the constructor.</param>
        /// <param name="env">The environment of the evaluation.</param>
        /// <param name="returnTo">The evaluator to return to.  This can be different from caller if this is the last step in evaluation</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next evaluator to excute.</returns>
        internal override Evaluator Apply(SchemeObject args, Environment env, Evaluator returnTo, Evaluator caller)
        {
#if Check
            this.CheckArgCount(ListLength(args), args, "ClrConstructor", caller);
#endif
            Assembly assembly = this.classType.Assembly;
            object[] argArray = this.ToArgList(args, null, "ClrConstructor", caller);
            object res = assembly.CreateInstance(this.classType.FullName, false, BindingFlags.Default, null, argArray, null, null);
            returnTo.ReturnedExpr = ClrObject.New(res);
            return returnTo;
        }
        #endregion
    }
}