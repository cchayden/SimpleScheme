// <copyright file="SynchronousClrProcedure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using Obj = System.Object;

    // TODO write unit tests for static and non-static cases

    /// <summary>
    /// Handles normal synchronous CLR method calls.
    /// </summary>
    internal sealed class SynchronousClrProcedure : ClrProcedure
    {
        #region Constructor
        /// <summary>
        /// Initializes a new instance of the SynchronousClrProcedure class.
        /// </summary>
        /// <param name="targetClassName">The class of the object to invoke.</param>
        /// <param name="methodName">The method to invoke.</param>
        /// <param name="argClassNames">The types of each argument.</param>
        private SynchronousClrProcedure(Obj targetClassName, Obj methodName, object argClassNames)
            : base(targetClassName, methodName)
        {
            try
            {
                this.ArgClasses = ClassList(argClassNames);
                Type cls = ToClass(ClassName);
                if (cls == null)
                {
                    ErrorHandlers.Error("Bad class: can't load class " + ClassName);
                }
                else
                {
                    this.MethodInfo = cls.GetMethod(this.MethodName, this.ArgClasses.ToArray());
                    if (this.MethodInfo == null)
                    {
                        ErrorHandlers.Error("Cant get method: " + this.MethodName);
                    }
                }
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
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the sync clr procedure primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        internal static new void DefinePrimitives(Environment env)
        {
            const int MaxInt = int.MaxValue;
            env
                //// (methodasync <target-class-name> <method-name> <arg-class-name> ...)
                .DefinePrimitive(
                   "method",
                   (args, caller) => new SynchronousClrProcedure(SchemeString.AsString(First(args), false), SchemeString.AsString(Second(args), false), Rest(Rest(args))),
                    2,
                    MaxInt);
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
        /// <param name="env">The environment for the application.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next step to excute.</returns>
        internal override Stepper Apply(object args, Environment env, Stepper caller)
        {
            object target;
            object[] argArray;
            if (this.MethodInfo.IsStatic)
            {
                target = null;
                argArray = this.ToArgList(args, null);
            }
            else
            {
                target = First(args);
                argArray = this.ToArgList(Rest(args), null);
            }

            return caller.ContinueStep(this.MethodInfo.Invoke(target, argArray));
        }
        #endregion
    }
}