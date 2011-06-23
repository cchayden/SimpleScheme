// <copyright file="SynchronousClrProcedure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    // TODO write unit tests for static and non-static cases

    /// <summary>
    /// Handles normal synchronous CLR method calls.
    /// </summary>
    public class SynchronousClrProcedure : ClrProcedure
    {
        public SynchronousClrProcedure(string methodName, object targetClassName, object argClassNames)
            : base(methodName, targetClassName)
        {
            try
            {
                this.argClasses = ClassArray(argClassNames);
                Type cls = ToClass(className);
                this.methodInfo = cls.GetMethod(methodName, this.argClasses);
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

        /// <summary>
        /// Take a list of Type or type name elements and create a corresponding 
        ///   array of Type.
        /// </summary>
        /// <param name="args">A list of Type or type name elements.</param>
        /// <returns>An array of Type objects corresponding to the list.</returns>
        protected Type[] ClassArray(object args)
        {
            return ClassArray(args, Length(args));
        }

        /// <summary>
        /// Take a list of CLR method arguments and turn them into an array, suitable for
        ///   calling the method.
        /// Check to make sure the number and types of the arguments in the list match 
        ///    what is expected.
        /// </summary>
        /// <param name="args">A list of the method arguments.</param>
        /// <returns>An array of arguments for the method call.</returns>
        protected override object[] ToArray(object args)
        {
            return ToArray(args, Length(args));
        }

        // TODO mabye this could be moved to base class.

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
            return this.methodInfo.IsStatic ? 
                this.methodInfo.Invoke(null, this.ToArray(args)) : 
                this.methodInfo.Invoke(First(args), this.ToArray(Rest(args)));
        }
    }
}