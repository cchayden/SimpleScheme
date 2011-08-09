// <copyright file="SynchronousClrProcedure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Text;

    using Obj = System.Object;

    // TODO write unit tests for static and non-static cases

    /// <summary>
    /// Handles normal synchronous CLR method calls.
    /// Immutable class.
    /// </summary>
    public sealed class SynchronousClrProcedure : ClrProcedure
    {
        #region Constants
        /// <summary>
        /// The printable name of the synchronous clr procedure type.
        /// </summary>
        public new const string Name = "synchronous clr procedure";
        #endregion

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
                Type cls = TypePrimitives.ToClass(ClassName);
                this.MethodInfo = cls.GetMethod(this.MethodName, this.ArgClasses.ToArray());
                if (this.MethodInfo == null)
                {
                    ErrorHandlers.ClrError("Can't find method: " + ClassName + ":" + this.MethodName);
                    return;
                } 
                if (this.MethodInfo.IsStatic)
                {
                    this.MaxArgs = this.MinArgs = this.ArgClasses.Count;
                } else
                {
                    this.MaxArgs = this.MinArgs = this.ArgClasses.Count + 1;
                }
            }
            catch (TypeLoadException)
            {
                ErrorHandlers.ClrError("Bad class, can't load: " + ClassName);
            }
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Tests whether to given object is a synchronous CLR procedure.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a synchronous CLR procedure.</returns>
        public static new bool Is(Obj obj)
        {
            return obj is SynchronousClrProcedure;
        }

        /// <summary>
        /// Convert object to synchronous clr procedure.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as a synchronous clr procedure.</returns>
        public static new SynchronousClrProcedure As(Obj obj)
        {
            if (Is(obj))
            {
                return (SynchronousClrProcedure)obj;
            }

            ErrorHandlers.TypeError(Name, obj);
            return null;
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the sync clr procedure primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static new void DefinePrimitives(PrimitiveEnvironment env)
        {
            const int MaxInt = int.MaxValue;
            env
                //// (method <target-class-name> <method-name> <arg-class-name> ...)
                .DefinePrimitive(
                   "method",
                   (args, caller) => new SynchronousClrProcedure(
                       Printer.AsString(List.First(args), false), 
                       Printer.AsString(List.Second(args), false), 
                       List.Rest(List.Rest(args))),
                    2,
                    MaxInt)
                .DefinePrimitive(
                   "property-get",
                   (args, caller) => new SynchronousClrProcedure(
                       Printer.AsString(List.First(args), false), 
                       "get_" + Printer.AsString(List.Second(args), false), 
                       List.Rest(List.Rest(args))),
                    2)
                .DefinePrimitive(
                   "property-set",
                   (args, caller) => new SynchronousClrProcedure(
                       Printer.AsString(List.First(args), false), 
                       "set_" + Printer.AsString(List.Second(args), false), 
                       List.Rest(List.Rest(args))),
                    3)
                .DefinePrimitive(
                   "index-get",
                   (args, caller) => new SynchronousClrProcedure(
                       Printer.AsString(List.First(args), false), 
                       "get_Item", 
                       List.Rest(args)),
                    2)
                .DefinePrimitive(
                   "index-set",
                   (args, caller) => new SynchronousClrProcedure(
                       Printer.AsString(List.First(args), false), 
                       "set_Item", 
                       List.Rest(args)),
                    3);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write the synchronous clr procedure to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public override void AsString(bool quoted, StringBuilder buf)
        {
            buf.Append(Name + ": ");
            buf.Append(this.ToString());
        }

        /// <summary>
        /// Display the synchronous clr procedure as a string.  
        /// </summary>
        /// <returns>The string form of the procedure.</returns>
        public override string ToString()
        {
            return "<" + Name+ ">";
        }

        /// <summary>
        /// Apply the method to the given arguments.
        /// If the method is static, all arguments are passed to the method.
        /// Otherwise, the first argument is the class instance, and the rest are passed 
        ///    to the method.
        /// </summary>
        /// <param name="args">Arguments to pass to the method.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next evaluator to excute.</returns>
        public override Evaluator Apply(Obj args, Evaluator caller)
        {
            object target;
            object[] argArray;
            CheckArgs(args, "SynchronousClrProcedure");
            if (this.MethodInfo.IsStatic)
            {
                target = null;
                argArray = this.ToArgList(args, null);
            }
            else
            {
                target = List.First(args);
                argArray = this.ToArgList(List.Rest(args), null);
            }

            Obj res = this.MethodInfo.Invoke(target, argArray);
            res = res ?? Undefined.Instance;
            return caller.UpdateReturnedExpr(res);
        }
        #endregion
    }
}