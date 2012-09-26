// <copyright file="SynchronousClrProcedure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;

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
        /// <param name="argClassNames">The types of each argument.</param>
        public SynchronousClrProcedure(string targetClassName, string methodName, SchemeObject argClassNames)
            : base(targetClassName, methodName)
        {
            this.SetArgClasses(this.ClassList(argClassNames));
            this.SetMethodInfo(this.MethodName, this.ArgClasses);
            this.SetMinMax(this.ArgClasses.Count + (this.MethodInfo.IsStatic ? 0 : 1));
        }
        #endregion

        #region SchemeType Accessors
        /// <summary>
        /// Gets the name of the type.
        /// </summary>
        public override string TypeName
        {
            get { return ValueTypeName(ValueType.Char); }
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
                       First(args).ToString(),
                       Second(args).ToString(),
                       Rest(Rest(args))),
                    2,
                    MaxInt, 
                    ValueType.StringOrSymbol)
                //// (property-get <target-class-name> <property-name>)
                .DefinePrimitive(
                   "property-get",
                   (args, caller) => new SynchronousClrProcedure(
                       First(args).ToString(),
                       "get_" + Second(args).ToString(), 
                       Rest(Rest(args))),
                    2, 
                    ValueType.String)
                //// (property-set <target-class-name> <property-name> <arg-class-name>)
                .DefinePrimitive(
                   "property-set",
                   (args, caller) => new SynchronousClrProcedure(
                       First(args).ToString(), 
                       "set_" + Second(args).ToString(), 
                       Rest(Rest(args))),
                    3, 
                    ValueType.String)
                //// (index-get <target-class-name> <arg-class-name> <index-type>)
                .DefinePrimitive(
                   "index-get",
                   (args, caller) => new SynchronousClrProcedure(
                       First(args).ToString(), 
                       "get_Item", 
                       Rest(args)),
                    2, 
                    ValueType.String)
                //// (index-set <target-class-name> <arg-class-name> <index-type> <arg-class-name>)
                .DefinePrimitive(
                   "index-set",
                   (args, caller) => new SynchronousClrProcedure(
                       First(args).ToString(), 
                       "set_Item",
                       Rest(args)),
                    3, 
                    ValueType.String);
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
            buf.Append(this.ToString());
        }

        /// <summary>
        /// Display the synchronous clr procedure as a string.  
        /// </summary>
        /// <returns>The string form of the procedure.</returns>
        public override string ToString()
        {
            return "<synchronous-clr-procedure>";
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
        public override Evaluator Apply(SchemeObject args, Evaluator caller)
        {
            this.CheckArgs(args, typeof(SynchronousClrProcedure));
            object target = null;
            if (!this.MethodInfo.IsStatic)
            {
                target = First(args);
                args = Rest(args);
            }

            if (target is ClrObject)
            {
                target = ((ClrObject)target).Value;
            }

            // If the target is a Symbol or SchemeString, then get the string
            // TODO cch do the same for Number, SchemeBoolean, etc.  Need a map like for arguments.
            if (target is Symbol || target is SchemeString)
            {
                target = target.ToString();
            }

            var argList = this.ToArgList(args, null);
            object res = this.MethodInfo.Invoke(target, argList);
            // TODO cch convert from CLR to Scheme
            res = res ?? Undefined.Instance;
            return caller.UpdateReturnValue(ClrObject.FromClrObject(res));
        }
        #endregion
    }
}