// <copyright file="AsynchronousClrProcedure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Diagnostics.Contracts;
    using System.Reflection;

    /// <summary>
    /// Handles asynchronous CLR method calls.
    /// Call returns suspended, then on completion resumes execution.
    /// This class is immutable.
    /// </summary>
    internal sealed class AsynchronousClrProcedure : ClrProcedure
    {
        #region Fields
        /// <summary>
        /// The method info for the EndXXX method.
        /// The caller class has info for BeginXXX.
        /// </summary>
        private readonly MethodInfo endMethodInfo;
        #endregion

        #region Constructors
        /// <summary>
        /// Initializes a new instance of the AsynchronousClrProcedure class.
        /// </summary>
        /// <param name="targetClassName">The class name of the CLR function.</param>
        /// <param name="methodName">The method name of the CLR function.</param>
        /// <param name="instanceClass">The type of the instance argument.  Null if static or constructor.</param>
        /// <param name="argClasses">The types of all method arguments.</param>
        /// <param name="caller">The calling evaluator.</param>
        internal AsynchronousClrProcedure(string targetClassName, string methodName, Type instanceClass, Type[] argClasses, Evaluator caller)
            : base(
                targetClassName, 
                methodName, 
                GetMethodInfo(targetClassName, "Begin" + methodName, argClasses, caller), 
                instanceClass,
                argClasses, 
                argClasses.Length - 1)
        {
            Contract.Requires(targetClassName != null);
            Contract.Requires(methodName != null);
            Contract.Requires(instanceClass != null);
            Contract.Requires(argClasses != null);
            Contract.Requires(caller != null);
            Contract.Assert(this.MethodInfo != null);
            var endClasses = new[] { typeof(IAsyncResult) };
            this.endMethodInfo = GetMethodInfo(targetClassName, "End" + methodName, endClasses, caller);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Display the asynchronous clr procedure as a string.  
        /// </summary>
        /// <returns>The string form of the procedure.</returns>
        public override string ToString()
        {
            return "<asynchronous-clr-procedure>";
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the async clr procedure primitives.
        /// </summary>
        /// <param name="primEnv">The environment to define the primitives into.</param>
        internal static new void DefinePrimitives(PrimitiveEnvironment primEnv)
        {
            Contract.Requires(primEnv != null);
            const int MaxInt = int.MaxValue;
            primEnv
                .DefinePrimitive(
                   "method-async", 
                   new[] { "(method-async <target-class-name> <method-name> <arg-class-name> ...)" },
                   (args, env, caller) => new AsynchronousClrProcedure(
                       First(args).ToString(), 
                       Second(args).ToString(), 
                       Class(First(args)),
                       ClassListBegin(Rest(Rest(args))), 
                       caller),
                   new ArgsInfo(2, MaxInt, ArgType.StringOrSymbol));
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
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next evaluator to execute.</returns>
        internal override Evaluator Apply(SchemeObject args, Evaluator returnTo, Evaluator caller)
        {
#if Check
            this.CheckArgCount(ListLength(args), args, "AsynchronousClrProcedure", caller);
#endif
            SchemeObject target = null;
            if (!this.MethodInfo.IsStatic)
            {
                target = First(args);
                Contract.Assert(target != null);
                args = Rest(args);
                Contract.Assert(args != null);
            }

            var actualTarget = ClrObject.ToClrObject(target, this.InstanceClass);
            var argArray = this.ToArgListBegin(args, new Tuple<object, Evaluator>(actualTarget, returnTo), caller);
            var res = this.MethodInfo.Invoke(actualTarget, argArray) as IAsyncResult;
            if (res == null)
            {
                ErrorHandlers.ClrError("Async method does not return IAsyncResult");
                return null;
            }

            // res is not converted because it is IAsyncResult -- convert in completion method
            return new SuspendedEvaluator(ClrObject.New(res), returnTo);
        }

        #endregion

        #region Private Methods
        /// <summary>
        /// Take a list of ArgType or type name elements and create a corresponding 
        ///   array of ArgType.
        /// Add the extra async arguments for a Begin method.
        /// </summary>
        /// <param name="args">A list of ArgType or type name elements.</param>
        /// <returns>An array of Types corresponding to the list.</returns>
        private static Type[] ClassListBegin(SchemeObject args)
        {
            Contract.Requires(args != null);
            Type[] array = ClassList(args, 2);
            array[array.Length - 2] = typeof(AsyncCallback);
            array[array.Length - 1] = typeof(object);
            return array;
        }

        /// <summary>
        /// Take a list of CLR method arguments and turn them into an array, suitable for
        ///   calling the method.
        /// Add the extra async arguments for the Begin call.
        /// Check to make sure the number and types of the arguments in the list match 
        ///    what is expected.
        /// </summary>
        /// <param name="args">A list of the method arguments.</param>
        /// <param name="state">State, passed on to completion function.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>An array of arguments for the method call.</returns>
        private object[] ToArgListBegin(SchemeObject args, object state, Evaluator caller)
        {
            Contract.Requires(args != null);
            Contract.Requires(state != null);
            Contract.Requires(caller != null);
            object[] additionalArgs = { (AsyncCallback)this.CompletionMethod, state };
            return this.ToArgList(args, additionalArgs, "AsynchronousClrProcedure", caller);
        }

        /// <summary>
        /// This is run when the async operation is complete.
        /// It sould run the "End" function, then resume stepping where it left off.
        /// </summary>
        /// <param name="result">The async result, used to get operation result.</param>
        private void CompletionMethod(IAsyncResult result)
        {
            Contract.Requires(result != null);
            Contract.Requires(result.AsyncState != null);
            Contract.Requires(this.endMethodInfo != null);
            object[] args = { result };
            var state = (Tuple<object, Evaluator>)result.AsyncState;
            Evaluator caller = state.Item2;
            Contract.Assert(caller != null);
            object res = this.endMethodInfo.Invoke(state.Item1, args);
            res = res ?? Undefined.Instance;
            caller.ReturnedExpr = ClrObject.FromClrObject(res);

            // Continue executing steps.  This thread takes over stepping
            //  because the other thread has already exited.
            caller.EvalStep();
        }
        #endregion
    }
}