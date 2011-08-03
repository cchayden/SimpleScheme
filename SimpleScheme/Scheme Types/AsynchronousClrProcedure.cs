// <copyright file="AsynchronousClrProcedure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;
    using System.Reflection;
    using System.Text;

    using Obj = System.Object;

    /// <summary>
    /// Handles asynchronous CLR method calls.
    /// Call returns suspended, then on completion resumes execution.
    /// This class is immutable.
    /// </summary>
    public sealed class AsynchronousClrProcedure : ClrProcedure
    {
        #region Constants
        /// <summary>
        /// The printable name of the asynchronous clr procedure type.
        /// </summary>
        public new const string Name = "asynchronous clr procedure";
        #endregion

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
        /// <param name="argClassNames">The types of all method arguments.</param>
        private AsynchronousClrProcedure(Obj targetClassName, Obj methodName, Obj argClassNames)
            : base(targetClassName, methodName)
        {
            this.ArgClasses = this.ClassListBegin(argClassNames);
            Type cls = TypePrimitives.ToClass(ClassName);
            if (cls == null)
            {
                ErrorHandlers.ClrError("Bad class: can't get type: " + ClassName);
                return; // actually Error throws an exception
            }

            this.MethodInfo = cls.GetMethod("Begin" + this.MethodName, this.ArgClasses.ToArray());
            if (this.MethodInfo == null)
            {
                ErrorHandlers.ClrError("Can't get BeginXXX method: " + this.MethodName);
            }

            Type[] endClasses = { typeof(IAsyncResult) };
            this.endMethodInfo = cls.GetMethod("End" + this.MethodName, endClasses);
            if (this.endMethodInfo == null)
            {
                ErrorHandlers.ClrError("Can't get EndXXX method: " + this.MethodName);
            }
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Tests whether to given object is an asynchronous CLR procedure.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a synchronous CLR procedure.</returns>
        public static bool IsAsynchronousClrProcedure(Obj obj)
        {
            return obj is AsynchronousClrProcedure;
        }

        /// <summary>
        /// Convert object to asynchronous clr procedure.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as a asynchronous clr procedure.</returns>
        public static AsynchronousClrProcedure AsAsynchronousClrProcedure(Obj obj)
        {
            if (IsAsynchronousClrProcedure(obj))
            {
                return (AsynchronousClrProcedure)obj;
            }

            ErrorHandlers.TypeError(Name, obj);
            return null;
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Display the asynchronous clr procedure as a string.  
        /// </summary>
        /// <returns>The string form of the procedure.</returns>
        public override string ToString()
        {
            return Name;
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the async clr procedure primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static new void DefinePrimitives(PrimitiveEnvironment env)
        {
            const int MaxInt = int.MaxValue;
            env
                //// (method-async <target-class-name> <method-name> <arg-class-name> ...)
                .DefinePrimitive(
                   "method-async",
                   (args, caller) => new AsynchronousClrProcedure(List.First(args), List.Second(args), List.Rest(List.Rest(args))),
                   2,
                   MaxInt);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Apply the method to the given arguments.
        /// If the method is static, all arguments are passed to the method.
        /// Otherwise, the first argument is the class instance, and the rest are passed 
        ///    to the method.
        /// </summary>
        /// <param name="args">Arguments to pass to the method.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next step to execute.</returns>
        public override Stepper Apply(object args, Stepper caller)
        {
            Obj target;
            Obj[] argArray;
            if (this.MethodInfo.IsStatic)
            {
                target = Undefined.Instance;
                argArray = this.ToArgListBegin(args, new AsyncState(target, caller));
            }
            else
            {
                target = List.First(args);
                argArray = this.ToArgListBegin(List.Rest(args), new AsyncState(target, caller));
            }

            IAsyncResult res = this.MethodInfo.Invoke(target, argArray) as IAsyncResult;
            return Stepper.NewSuspended(res);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Take a list of Type or type name elements and create a corresponding 
        ///   array of Type.
        /// Add the extra async arguments for a Begin method.
        /// </summary>
        /// <param name="args">A list of Type or type name elements.</param>
        /// <returns>An array of Types corresponding to the list.</returns>
        private List<Type> ClassListBegin(Obj args)
        {
            List<Type> array = ClassList(args);
            array.Add(TypePrimitives.ToClass("System.AsyncCallback"));
            array.Add(TypePrimitives.ToClass("System.Object"));

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
        private object[] ToArgListBegin(object args, object state)
        {
            object[] additionalArgs = { (AsyncCallback)this.CompletionMethod, state };
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
            Stepper caller = state.Caller;
            Obj res = this.endMethodInfo.Invoke(state.InvokedObject, args);
            caller.ContinueStep(res);

            // Continue executing steps.  This thread takes over stepping
            //  because the other thread has already exited.
            caller.EvalStep();
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
            /// <param name="caller">The caller stepper to resume.</param>
            public AsyncState(object invokedObject, Stepper caller)
            {
                this.InvokedObject = invokedObject;
                this.Caller = caller;
            }

            /// <summary>
            /// Gets the object to invoke the end method on.
            /// </summary>
            public object InvokedObject { get; private set; }

            /// <summary>
            /// Gets the caller stepper to resume.
            /// </summary>
            public Stepper Caller { get; private set; }
        }
        #endregion
    }

    #region Extensions
    /// <summary>
    /// Provide common operations as extensions.
    /// </summary>
    public static partial class Extensions
    {
        /// <summary>
        /// Write the asynchronous clr procedure to the string builder.
        /// </summary>
        /// <param name="proc">The asynchronous clr procedure.</param>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public static void AsString(this AsynchronousClrProcedure proc, bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append("asynchronous procedure: ");
                buf.Append(proc.ToString());
            }
        }
    }
    #endregion
}