// <copyright file="AsynchronousClrProcedure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;
    using System.Reflection;

    /// <summary>
    /// Handles asynchronous CLR method calls.
    /// Call returns suspended, then on completion resumes execution.
    /// </summary>
    public class AsynchronousClrProcedure : ClrProcedure
    {
        /// <summary>
        /// The method info for the EndXXX method.
        /// The parent class has info for BeginXXX.
        /// </summary>
        private readonly MethodInfo endMethodInfo;

        /// <summary>
        /// Initializes a new instance of the AsynchronousClrProcedure class.
        /// </summary>
        /// <param name="targetClassName">The class name of the CLR function.</param>
        /// <param name="methodName">The method name of the CLR function.</param>
        /// <param name="argClassNames">The types of all method arguments.</param>
        public AsynchronousClrProcedure(object targetClassName, string methodName, object argClassNames)
            : base(methodName, targetClassName)
        {
            try
            {
                this.ArgClasses = this.ClassListBegin(argClassNames);
                Type cls = ToClass(ClassName);
                if (cls == null)
                {
                    Error("Bad class: can't load class " + ClassName);
                    return;    // actually Error throws an exception
                }

                this.MethodInfo = cls.GetMethod("Begin" + methodName, this.ArgClasses.ToArray());

                Type[] endClasses = { typeof(IAsyncResult) };
                this.endMethodInfo = cls.GetMethod("End" + methodName, endClasses);
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

        // TODO if this can be static or non-static, move synchronous version to base class and share.

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
            object invokedObj = First(args);
            AsyncState state = new AsyncState(invokedObj, parent);
            object[] argArray = this.ToArgListBegin(Rest(args), state).ToArray();
            IAsyncResult res = (IAsyncResult)this.MethodInfo.Invoke(invokedObj, argArray);
            return Stepper.Suspended;
        }

        /// <summary>
        /// Take a list of Type or type name elements and create a corresponding 
        ///   array of Type.
        /// Add the extra async arguments for a Begin method.
        /// </summary>
        /// <param name="args">A list of Type or type name elements.</param>
        /// <returns>An array of Type objects corresponding to the list.</returns>
        private List<Type> ClassListBegin(object args)
        {
            List<Type> array = ClassList(args);
            array.Add(ToClass("System.AsyncCallback"));
            array.Add(ToClass("object"));

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
        /// <returns>An array of arguments for the method call.</returns>
        private List<object> ToArgListBegin(object args, object state)
        {
            AsyncCallback cm = this.CompletionMethod;
            object[] additionalArgs = { cm, state };
            return ToArgList(args, additionalArgs);
        }

        /// <summary>
        /// This is run when the async operation is complete.
        /// It sould run the "End" function, then resume stepping where it left off.
        /// </summary>
        /// <param name="result">The async result, used to get operation result.</param>
        private void CompletionMethod(IAsyncResult result)
        {
            object[] args = { result };
            AsyncState state = (AsyncState)result.AsyncState;
            Stepper parent = state.Parent;
            object res = this.endMethodInfo.Invoke(state.InvokedObject, args);
            parent.ReturnedExpr = res;
            parent.Env.Interp.EvalStep(parent);
        }

        /// <summary>
        /// Stores the async state needed to resume execution from the callback.
        /// </summary>
        private class AsyncState
        {
            /// <summary>
            /// Initializes a new instance of the AsynchronousClrProcedure.AsyncState class.
            /// </summary>
            /// <param name="invokedObject">The object on which the EndXXX must be invoked.</param>
            /// <param name="parent">The parent stepper to resume.</param>
            public AsyncState(object invokedObject, Stepper parent)
            {
                this.InvokedObject = invokedObject;
                this.Parent = parent;
            }

            /// <summary>
            /// Gets the object to invoke the end method on.
            /// </summary>
            public object InvokedObject { get; private set; }

            /// <summary>
            /// Gets the parent stepper to resume.
            /// </summary>
            public Stepper Parent { get; private set; }
        }
    }
}