// <copyright file="SynchronousClrProcedure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using Obj = System.Object;

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
        public new const string Name = "synchronous-clr-procedure";
        #endregion

        /// <summary>
        /// The printable name of this scheme type.
        /// </summary>
        public static new string TypeName = Primitive.ValueType.SynchronousClrProcedure.ToString();

        /// <summary>
        /// Identifies objects of this scheme type.
        /// </summary>
        /// <param name="obj">The object to test.</param>
        /// <returns>True if the object is this scheme type.</returns>
        public new static bool Is(Obj obj)
        {
            return obj is SynchronousClrProcedure;
        }

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the SynchronousClrProcedure class.
        /// </summary>
        /// <param name="targetClassName">The class of the object to invoke.</param>
        /// <param name="methodName">The method to invoke.</param>
        /// <param name="argClassNames">The types of each argument.</param>
        private SynchronousClrProcedure(Obj targetClassName, Obj methodName, Obj argClassNames)
            : base(targetClassName, methodName)
        {
            this.SetArgClasses(this.ClassList(argClassNames));
            this.SetMethodInfo(this.MethodName, this.ArgClasses);
            this.SetMinMax(this.ArgClasses.Count + (this.MethodInfo.IsStatic ? 0 : 1));
        }

        #endregion

        #region Public Static Methods
        /// <summary>
        /// Creates a new instance of the SynchronousClrProcedure class.
        /// </summary>
        /// <param name="targetClassName">The class of the object to invoke.</param>
        /// <param name="methodName">The method to invoke.</param>
        /// <param name="argClassNames">The types of each argument.</param>
        /// <returns>The SynchronousClrProcedure.</returns>
        public static SynchronousClrProcedure New(Obj targetClassName, Obj methodName, Obj argClassNames)
        {
            return new SynchronousClrProcedure(targetClassName, methodName, argClassNames);
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
                   Symbol.New("method"),
                   (args, caller) => New(
                       Printer.AsString(args.First(), false), 
                       Printer.AsString(args.Second(), false), 
                       args.Rest().Rest()),
                    2,
                    MaxInt, 
                    Primitive.ValueType.String)
                //// (property-get <target-class-name> <property-name>)
                .DefinePrimitive(
                   Symbol.New("property-get"),
                   (args, caller) => New(
                       Printer.AsString(args.First(), false), 
                       "get_" + Printer.AsString(args.Second(), false), 
                       args.Rest().Rest()),
                    2, 
                    Primitive.ValueType.String)
                //// (property-set <target-class-name> <property-name> <arg-class-name>)
                .DefinePrimitive(
                   Symbol.New("property-set"),
                   (args, caller) => New(
                       Printer.AsString(args.First(), false), 
                       "set_" + Printer.AsString(args.Second(), false), 
                       args.Rest().Rest()),
                    3, 
                    Primitive.ValueType.String)
                //// (index-get <target-class-name> <arg-class-name> <index-type>)
                .DefinePrimitive(
                   Symbol.New("index-get"),
                   (args, caller) => New(
                       Printer.AsString(args.First(), false), 
                       "get_Item", 
                       args.Rest()),
                    2, 
                    Primitive.ValueType.String)
                //// (index-set <target-class-name> <arg-class-name> <index-type> <arg-class-name>)
                .DefinePrimitive(
                   Symbol.New("index-set"),
                   (args, caller) => New(
                       Printer.AsString(args.First(), false), 
                       "set_Item", 
                       args.Rest()),
                    3, 
                    Primitive.ValueType.String);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write the synchronous clr procedure to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public new void PrintString(bool quoted, StringBuilder buf)
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
            return "<" + Name + ">";
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
            CheckArgs(args, "SynchronousClrProcedure");
            object target = null;
            if (!this.MethodInfo.IsStatic)
            {
                target = args.First();
                args = args.Rest();
            }

            if (target != null && target.IsSymbol())
            {
                target = target.ToString();
            }

            var res = this.MethodInfo.Invoke(target, this.ToArgList(args, null));
            res = res ?? Undefined.New();
            return caller.UpdateReturnValue(res);
        }
        #endregion
    }

    /// <summary>
    /// Extension class for SynchronousClrProcedure
    /// </summary>
    public static class SynchronousClrProcedureExtension
    {
        /// <summary>
        /// Tests whether to given object is a synchronous CLR procedure.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a synchronous CLR procedure.</returns>
        public static bool IsSynchronousClrProcedure(this Obj obj)
        {
            return SynchronousClrProcedure.Is(obj);
        }

        /// <summary>
        /// Convert object to synchronous clr procedure.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as a synchronous clr procedure.</returns>
        public static SynchronousClrProcedure AsSynchronousClrProcedure(this Obj obj)
        {
            if (SynchronousClrProcedure.Is(obj))
            {
                return (SynchronousClrProcedure)obj;
            }

            ErrorHandlers.TypeError(SynchronousClrProcedure.Name, obj);
            return null;
        }
    }
}