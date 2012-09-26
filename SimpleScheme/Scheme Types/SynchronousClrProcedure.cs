// <copyright file="SynchronousClrProcedure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Handles normal synchronous CLR method calls.
    /// Immutable class.
    /// </summary>
    public sealed class SynchronousClrProcedure : ClrProcedure
    {
        #region Constructor
        /// <summary>
        /// Initializes a new instance of the SynchronousClrProcedure class.
        /// </summary>
        /// <param name="targetClassName">The class of the object to invoke.</param>
        /// <param name="methodName">The method to invoke.</param>
        /// <param name="instanceClass">The type of the instance argument.  Null if static or constructor.</param>
        /// <param name="argClasses">The types of each argument.</param>
        /// <param name="caller">The calling evaluator.</param>
        private SynchronousClrProcedure(string targetClassName, string methodName, Type instanceClass, Type[] argClasses, Evaluator caller)
            : base(
                targetClassName, 
                methodName, 
                GetMethodInfo(targetClassName, methodName, argClasses, caller), 
                instanceClass,
                argClasses, 
                argClasses.Length)
        {
            Contract.Requires(targetClassName != null);
            Contract.Requires(methodName != null);
            Contract.Requires(instanceClass != null);
            Contract.Requires(argClasses != null);
            Contract.Requires(caller != null);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Display the synchronous clr procedure as a string.  
        /// </summary>
        /// <returns>The string form of the procedure.</returns>
        public override string ToString()
        {
            return "<synchronous-clr-procedure>";
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
                   "method", 
                   new[] { "(method <target-class-name> <method-name> <arg-class-name> ...)" },
                   (args, env, caller) => new SynchronousClrProcedure(
                                         First(args).ToString(),
                                         Second(args).ToString(),
                                         Class(First(args)),
                                         ClassList(Rest(Rest(args))), 
                                         caller),
                    new ArgsInfo(1, MaxInt, ArgType.StringOrSymbol))
                .DefinePrimitive(
                   "property-get", 
                   new[] { "(property-get <target-class-name> <property-name>)" },
                   (args, env, caller) => new SynchronousClrProcedure(
                                         First(args).ToString(),
                                         "get_" + Second(args).ToString(), 
                                         Class(First(args)),
                                         ClassList(Rest(Rest(args))), 
                                         caller),
                    new ArgsInfo(2, ArgType.StringOrSymbol))
                .DefinePrimitive(
                   "property-set", 
                   new[] { "(property-set <target-class-name> <property-name> <arg-class-name>)" },
                   (args, env, caller) => new SynchronousClrProcedure(
                                         First(args).ToString(), 
                                         "set_" + Second(args).ToString(), 
                                         Class(First(args)),
                                         ClassList(Rest(Rest(args))), 
                                         caller),
                    new ArgsInfo(3,  ArgType.StringOrSymbol))
                .DefinePrimitive(
                   "index-get", 
                   new[] { "(index-get <target-class-name> <arg-class-name> <index-type>)" },
                   (args, env, caller) => new SynchronousClrProcedure(
                                         First(args).ToString(), 
                                         "get_Item", 
                                         Class(First(args)),
                                         ClassList(Rest(args)), 
                                         caller),
                    new ArgsInfo(2, ArgType.StringOrSymbol))
                .DefinePrimitive(
                   "index-set", 
                   new[] { "(index-set <target-class-name> <arg-class-name> <index-type> <arg-class-name>)" },
                   (args, env, caller) => new SynchronousClrProcedure(
                                         First(args).ToString(), 
                                         "set_Item",
                                         Class(First(args)),
                                         ClassList(Rest(args)), 
                                         caller),
                    new ArgsInfo(3, ArgType.StringOrSymbol));
        }
        #endregion

        #region Internal Methods

        /// <summary>
        /// Apply the method to the given arguments.
        /// If the method is static, all arguments are passed to the method.
        /// Otherwise, the first argument is the class instance, and the rest are passed 
        ///    to the method.
        /// </summary>
        /// <param name="args">Arguments to pass to the method.</param>
        /// <param name="returnTo">The evaluator to return to.  This can be different from caller if this is the last step in evaluation</param>
        /// <returns>The next evaluator to excute.</returns>
        internal override Evaluator Apply(SchemeObject args, Evaluator returnTo)
        {
            SchemeObject target = null;
            if (!this.MethodInfo.IsStatic)
            {
                target = First(args);
                args = Rest(args);
            }

#if Check
            this.CheckArgCount(ListLength(args), args, "SynchronousClrProcedure");
#endif

            var actualTarget = ClrObject.ToClrObject(target, this.InstanceClass);
            var argList = this.ToArgList(args, null, "SynchronousClrProcedure");
            object res = this.MethodInfo.Invoke(actualTarget, argList);
            res = res ?? Undefined.Instance;
            returnTo.ReturnedExpr = ClrObject.FromClrObject(res);
            return returnTo;
        }
        #endregion
    }
}