// <copyright file="AsynchronousClrProcedure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Reflection;

    /// <summary>
    /// Handles asynchronous CLR method calls.
    /// Call returns Delay, then on completion resumes execution.
    /// </summary>
    public class AsynchronousClrProcedure : ClrProcedure
    {
        delegate object EndResult(IAsyncResult result, object state);
        EndResult endResult;

        public AsynchronousClrProcedure(string methodName, object targetClassName, object argClassNames)
            : base(methodName, targetClassName)
        {
            try
            {
                this.argClasses = ClassArray(argClassNames);
                Type cls = ToClass(className);
                this.methodInfo = cls.GetMethod(methodName, this.argClasses);

                string endMethodName = "End" + methodName.Substring(5);
                Type[] endClasses = { typeof(IAsyncResult), typeof(object)};
                MethodInfo endMethodInfo = cls.GetMethod(endMethodName, endClasses);
                //endResult = xxx;
            }
            catch (TypeLoadException)
            {
                Error("Bad class, can't get method: " + this.Name);
            }
            catch (MissingMethodException)
            {
                Error("Can't get method: " + this.Name);
            }
        }

        // TOO if this can be static or non-static, move synchronous version to base class and share.
        /// <summary>
        /// Apply the method to the given arguments.
        /// If the method is static, all arguments are passed to the method.
        /// Otherwise, the first argument is the class instance, and the rest are passed 
        ///    to the method.
        /// </summary>
        /// <param name="parent">The calling evaluator.</param>
        /// <param name="args">Arguments to pass to the method.</param>
        /// <returns>The result of executing the method.</returns>
        public override object Apply(Stepper parent, object args)
        {
            return this.methodInfo.Invoke(First(args), this.ToArray(Rest(args)));
        }

        /// <summary>
        /// Take a list of Type or type name elements and create a corresponding 
        ///   array of Type.
        /// Add the extra async arguments.
        /// </summary>
        /// <param name="args">A list of Type or type name elements.</param>
        /// <returns>An array of Type objects corresponding to the list.</returns>
        protected Type[] ClassArray(object args)
        {
            int n = Length(args);
            Type[] array = ClassArray(args, n + 2);
            array[n] = ToClass("IAsyncResult");
            array[n + 1] = typeof(object);

            return array;
        }

        /// <summary>
        /// Take a list of CLR method arguments and turn them into an array, suitable for
        ///   calling the method.
        /// Add the extra async arguments.
        /// Check to make sure the number and types of the arguments in the list match 
        ///    what is expected.
        /// </summary>
        /// <param name="args">A list of the method arguments.</param>
        /// <returns>An array of arguments for the method call.</returns>
        protected override object[] ToArray(object args)
        {
            int n = Length(args);
            object[] array = ToArray(args, n + 2);
            //array[n] = CompletionMethod;
            array[n + 1] = null;

            return array;
        }

        /// <summary>
        /// This is run when the async operation is complete.
        /// It sould run the "End" function, then resume stepping where it left off.
        /// </summary>
        /// <param name="result">The async result, used to get operation result.</param>
        /// <param name="state">The async state.</param>
        public void CompletionMethod(IAsyncResult result, object state)
        {
            object res = this.endResult(result, state);
        }
    }
}