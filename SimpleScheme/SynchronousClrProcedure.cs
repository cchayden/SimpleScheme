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
    public sealed class SynchronousClrProcedure : ClrProcedure
    {
        /// <summary>
        /// Initializes a new instance of the SynchronousClrProcedure class.
        /// </summary>
        /// <param name="targetClassName">The class of the object to invoke.</param>
        /// <param name="methodName">The method to invoke.</param>
        /// <param name="argClassNames">The types of each argument.</param>
        public SynchronousClrProcedure(object targetClassName, string methodName, object argClassNames)
            : base(methodName, targetClassName)
        {
            try
            {
                this.ArgClasses = ClassList(argClassNames);
                Type cls = ToClass(ClassName);
                if (cls == null)
                {
                    ErrorHandlers.Error("Bad class: can't load class " + ClassName);
                }

                this.MethodInfo = cls.GetMethod(methodName, this.ArgClasses.ToArray());
            }
            catch (TypeLoadException)
            {
                ErrorHandlers.Error("Bad class, can't get method: " + this.Name);
            }
            catch (MissingMethodException)
            {
                ErrorHandlers.Error("Can't get method: " + this.Name);
            }
        }

        // TODO maybe this could be moved to base class.

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
#if DEBUG
            object target;
            object[] argArray;
            if (this.MethodInfo.IsStatic)
            {
                target = null;
                argArray = this.ToArgList(args, null).ToArray();
            }
            else
            {
                target = List.First(args);
                argArray = this.ToArgList(List.Rest(args), null).ToArray();
            }

            return this.MethodInfo.Invoke(target, argArray);
#else
            return this.methodInfo.IsStatic ? 
                this.methodInfo.Invoke(null, this.ToArgList(args).ToArray()) : 
                this.methodInfo.Invoke(First(args), this.ToArgList(Rest(args)).ToArray());
#endif
        }
    }
}