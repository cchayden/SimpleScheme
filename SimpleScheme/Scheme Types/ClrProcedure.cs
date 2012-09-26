// <copyright file="ClrProcedure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;
    using System.Reflection;
    using System.Text;

    /// <summary>
    /// Executes a function provided in the CLR.
    /// This class is immutable.  All attributes are set in constructors, either in this
    ///   class or in its subclasses.
    /// </summary>
    public abstract class ClrProcedure : Procedure
    {
        #region Constructor
        /// <summary>
        /// Initializes a new instance of the ClrProcedure class.
        /// This allows calls into CLR methods.
        /// The name of the method and the class that it is found in must be supplied.
        /// Also, a list of the types of the method arguments must be given.
        /// The className and methodName can be strings or symbols, but symbols are converted to
        ///   lower case by the reader, so this is not very useful.
        /// </summary>
        /// <param name="className">The name of the class containing the method.</param>
        /// <param name="methodName">The name of the CLR method.</param>
        protected ClrProcedure(string className, string methodName) :
            base(0, 0)
        {
            this.ClassName = className;
            this.MethodName = methodName;
            this.SetName(className + "." + methodName);
        }
        #endregion

        #region SchemeType Accessors
        /// <summary>
        /// Gets the name of the type.
        /// </summary>
        public override string TypeName
        {
            get { return ValueTypeName(ValueType.SynchronousClrProcedure); }
        }
        #endregion

        #region Accessors

        /// <summary>
        /// Gets the name of the class containing the method to invoke.
        /// </summary>
        protected string ClassName { get; private set; }

        /// <summary>
        /// Gets the name of the method.
        /// </summary>
        protected string MethodName { get; private set; }

        /// <summary>
        /// Gets information about the CLR method to be called.
        /// </summary>
        protected MethodInfo MethodInfo { get; private set; }

        /// <summary>
        /// Gets the types of the arguments.
        /// </summary>
        protected List<Type> ArgClasses { get; private set; }

        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the clr procedure primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static new void DefinePrimitives(PrimitiveEnvironment env)
        {
            env
                //// (class <class-name>)
                .DefinePrimitive(
                    "class",
                    (args, caller) => ClrObject.New(Class(First(args).ToString())), 
                    1, 
                    ValueType.String)
                //// (new <class-name>)
                .DefinePrimitive(
                    "new",
                    (args, caller) => ClrObject.New(New(First(args).ToString())), 
                    1, 
                    ValueType.String)
                //// (new-array <class-name> <length>)
                .DefinePrimitive(
                    "new-array",
                    (args, caller) => ClrObject.New(NewArray(First(args).ToString(), (Number)Second(args))), 
                    2, 
                    ValueType.String, 
                    ValueType.Number);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write the clr procedure to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public new void PrintString(bool quoted, StringBuilder buf)
        {
            buf.Append("clr-procedure: " + this);
        }

        /// <summary>
        /// Display the CLR proc name as a string.  
        /// </summary>
        /// <returns>The string form of the continuation.</returns>
        public override string ToString()
        {
            return string.Format("ClrProcedure {0}", this.ProcedureName);
        }
        #endregion

        #region Protected Methods
        /// <summary>
        /// Look up the method info.
        /// </summary>
        /// <param name="theMethodName">The method name</param>
        /// <param name="argClassList">The argument types.</param>
        /// <returns>The method info for the method.</returns>
        protected MethodInfo GetMethodInfo(string theMethodName, List<Type> argClassList)
        {
            try
            {
                Type cls = this.ClassName.ToClass();
                if (cls == null)
                {
                    ErrorHandlers.ClrError("Can't find class: " + this.ClassName);
                    return null;
                }

                MethodInfo info = cls.GetMethod(theMethodName, argClassList.ToArray());
                if (info == null)
                {
                    ErrorHandlers.ClrError("Can't find method: " + this.ClassName + ":" + theMethodName);
                    return null;
                }

                return info;
            }
            catch (TypeLoadException)
            {
                ErrorHandlers.ClrError("Bad class, can't load: " + this.ClassName);
                return null;
            }
        }

        /// <summary>
        /// Look up the method info and store it.
        /// Only called from within subclass constructors.
        /// </summary>
        /// <param name="theMethodName">The method name</param>
        /// <param name="argClassList">The argument types.</param>
        protected void SetMethodInfo(string theMethodName, List<Type> argClassList)
        {
            this.MethodInfo = this.GetMethodInfo(theMethodName, argClassList);
        }

        /// <summary>
        /// Sets the list of classes for the procedure arguments.
        /// Only called from within subclass constructors.
        /// </summary>
        /// <param name="argClassList">The list of argument classes.</param>
        protected void SetArgClasses(List<Type> argClassList)
        {
            this.ArgClasses = argClassList;
        }

        /// <summary>
        /// Take a list of ValueType or type name elements and create a corresponding 
        ///   List of ValueType.
        /// </summary>
        /// <param name="args">A list of ValueType or type name elements.  There may be more than this.</param>
        /// <returns>An array of Types corresponding to the list.</returns>
        protected List<Type> ClassList(SchemeObject args)
        {
            int n = ListLength(args);
            var array = new List<Type>(n);

            while (args is Pair)
            {
                array.Add((First(args)).ToClass());
                args = Rest(args);
            }

            return array;
        }

        /// <summary>
        /// Take a list of CLR method arguments and turn them into a list, suitable for
        ///   calling the method.
        /// Check to make sure the number and types of the arguments in the list match 
        ///    what is expected.
        /// </summary>
        /// <param name="args">A list of the method arguments.</param>
        /// <param name="additionalArgs">A list of the additional args, not supplied by the caller. 
        /// These are part of the asynchronous calling pattern.</param>
        /// <returns>An array of arguments for the method call.</returns>
        protected object[] ToArgList(SchemeObject args, object[] additionalArgs)
        {
            int n = ListLength(args);
            int additionalN = additionalArgs != null ? additionalArgs.Length : 0;
            int diff = n + additionalN - this.ArgClasses.Count;
            if (diff != 0)
            {
                ErrorHandlers.SemanticError(Math.Abs(diff) + 
                    " too " + (diff > 0 ? "many" : "few") + 
                    " args to " + this.ProcedureName);
            }

            var array = new object[n + additionalN];

            int a = 0;
            while (args is Pair)
            {
                array[a] = ClrObject.ToClrObject(First(args), this.ArgClasses[a]);
                a++;
                args = Rest(args);
            }

            if (additionalArgs != null)    
            {
                for (int i = 0; i < additionalN; i++)
                {
                    array[a++] = additionalArgs[i];
                }
            }

            return array;
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Create a type object.
        /// Let any exceptions propogate up.
        /// </summary>
        /// <param name="className">The class name.  May have to be assembly-qualified.</param>
        /// <returns>The type object.</returns>
        private static Type Class(string className)
        {
            return className.ToClass();
        }

        /// <summary>
        /// Create a new object instance.
        /// This requires the class to have a default constructor.
        /// The class name must be assembly-qualified, unless it is in a standard assembly.
        /// These can throw a variety of exceptions.  Let them propogate up and be caught at
        ///   a higher level.
        /// </summary>
        /// <param name="className">The class name.</param>
        /// <returns>A new instance of the class.</returns>
        private static object New(string className)
        {
            Type type = className.ToClass();
            if (type == null)
            {
                ErrorHandlers.ClrError("ValueType cannot be found: " + className);
                return null;
            }

            Assembly assembly = type.Assembly;
            return assembly.CreateInstance(type.FullName);
        }

        /// <summary>
        /// Create an array of objects.
        /// These can throw a variety of exceptions.  Let them propogate up and be caught at
        ///   a higher level.
        /// </summary>
        /// <param name="className">The class name of the array elements.</param>
        /// <param name="length">The array length.</param>
        /// <returns>An array of the given length.</returns>
        private static object NewArray(string className, SchemeObject length)
        {
            Type type = className.ToClass();
            return Array.CreateInstance(type, Number.AsInt(length));
        }
        #endregion
    }
}