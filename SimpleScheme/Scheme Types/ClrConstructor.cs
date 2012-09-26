// <copyright file="ClrConstructor.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Reflection;
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Handles  CLR method constructor calls.
    /// This is used when it is necessary to create instances with non-default constructor.
    /// Immutable class.
    /// </summary>
    public sealed class ClrConstructor : ClrProcedure
    {
        #region Fields
        /// <summary>
        /// The type of the class to construct.
        /// </summary>
        private readonly Type classType;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the ClrConstructor class.
        /// </summary>
        /// <param name="targetClassName">The class of the object to invoke.</param>
        /// <param name="argClassNames">The types of each argument.</param>
        private ClrConstructor(Obj targetClassName, Obj argClassNames)
            : base(targetClassName, ".ctor")
        {
            try
            {
                this.classType = TypePrimitives.ToClass(targetClassName);
                this.SetArgClasses(this.ClassList(argClassNames));
                this.SetMinMax(this.ArgClasses.Count);
            }
            catch (TypeLoadException)
            {
                ErrorHandlers.ClrError("Bad class, can't load: " + Printer.AsString(targetClassName, false));
            }

            if (this.classType == null)
            {
                ErrorHandlers.ClrError("ValueType cannot be found: " + Printer.AsString(targetClassName, false));
            }
        }
        #endregion

        #region SchemeType Accessors
        /// <summary>
        /// Gets the name of the type.
        /// </summary>
        public override string TypeName
        {
            get { return TypePrimitives.ValueTypeName(TypePrimitives.ValueType.ClrConstructor); }
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Identifies objects of this scheme type.
        /// </summary>
        /// <param name="obj">The object to test.</param>
        /// <returns>True if the object is this scheme type.</returns>
        public static new bool Is(Obj obj)
        {
            return obj is ClrConstructor;
        }

        /// <summary>
        /// Creates a new instance of the ClrConstructor class.
        /// </summary>
        /// <param name="targetClassName">The class of the object to invoke.</param>
        /// <param name="argClassNames">The types of each argument.</param>
        /// <returns>A new ClrConstructor.</returns>
        public static ClrConstructor New(Obj targetClassName, Obj argClassNames)
        {
            return new ClrConstructor(targetClassName, argClassNames);
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
                //// (constructor <class-name> <arg-class-name> ...)
                .DefinePrimitive(
                    Symbol.New("constructor"),
                    (args, caller) => New(Printer.AsString(args.First(), false), args.Rest()),
                    1,
                    MaxInt, 
                    TypePrimitives.ValueType.String);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write the clr constructor to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public new void PrintString(bool quoted, StringBuilder buf)
        {
            buf.Append(this.ToString());
        }

        /// <summary>
        /// Display the clr constructor as a string.  
        /// </summary>
        /// <returns>The string form of the constructor.</returns>
        public override string ToString()
        {
            return "<clr-constructor>";
        }

        /// <summary>
        /// Execute the constructor.
        /// Match all arguments supplied up to the constructor's types.
        /// </summary>
        /// <param name="args">Arguments to pass to the constructor.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next evaluator to excute.</returns>
        public override Evaluator Apply(Obj args, Evaluator caller)
        {
            this.CheckArgs(args, typeof(ClrConstructor));
            Assembly assembly = this.classType.Assembly;
            object[] argArray = ToArgList(args, null);
            Obj res = assembly.CreateInstance(this.classType.FullName, false, BindingFlags.Default, null, argArray, null, null);
            res = res ?? Undefined.New();
            return caller.UpdateReturnValue(res);
        }
        #endregion
    }

    #region Extension Class
    /// <summary>
    /// Extensions for ClrConstructor
    /// </summary>
    public static class ClrConstructorExtension
    {
        /// <summary>
        /// Tests whether to given object is a CLR constructor.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a CLR constructor.</returns>
        public static bool IsClrConstructor(this Obj obj)
        {
            return ClrConstructor.Is(obj);
        }

        /// <summary>
        /// Convert object to clr constructor.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as a clr constructor.</returns>
        public static ClrConstructor AsClrConstructor(this Obj obj)
        {
            if (ClrConstructor.Is(obj))
            {
                return (ClrConstructor)obj;
            }

            ErrorHandlers.TypeError(typeof(ClrConstructor), obj);
            return null;
        }
    }
    #endregion
}