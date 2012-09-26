﻿// <copyright file="ClrConstructor.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Reflection;
    using System.Text;

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
        private ClrConstructor(ISchemeObject targetClassName, ISchemeObject argClassNames)
            : base(targetClassName, (Symbol)".ctor")
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
                    "constructor",
                    (args, caller) => new ClrConstructor((Symbol)Printer.AsString(List.First(args), false), List.Rest(args)),
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
        public override Evaluator Apply(ISchemeObject args, Evaluator caller)
        {
            this.CheckArgs(args, typeof(ClrConstructor));
            Assembly assembly = this.classType.Assembly;
            object[] argArray = ToArgList(args, null);
            object res = assembly.CreateInstance(this.classType.FullName, false, BindingFlags.Default, null, argArray, null, null);
            res = res ?? Undefined.Instance;
            return caller.UpdateReturnValue(ClrObject.New(res));
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
        /// Convert object to clr constructor.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as a clr constructor.</returns>
        public static ClrConstructor AsClrConstructor(this ISchemeObject obj)
        {
            if (obj is ClrConstructor)
            {
                return (ClrConstructor)obj;
            }

            ErrorHandlers.TypeError(typeof(ClrConstructor), obj);
            return null;
        }
    }
    #endregion
}