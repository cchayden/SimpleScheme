// <copyright file="ClrProcedure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Diagnostics;
    using System.Reflection;

    /// <summary>
    /// Executes a function provided in the CLR.
    /// This class is immutable.  All attributes are set in constructors, either in this
    ///   class or in its subclasses.
    /// </summary>
    public abstract class ClrProcedure : Procedure
    {
        #region Fields
        /// <summary>
        /// The class name of the CLR class.
        /// </summary>
        private readonly string className;

        /// <summary>
        /// Information about the CLR method.
        /// </summary>
        private readonly MethodInfo methodInfo;

        /// <summary>
        /// The class name of the instance for an instance method.
        /// Null for static methods and constructors.
        /// </summary>
        private readonly Type instanceClass;

        /// <summary>
        /// The types of each of the arguments to the method.
        /// </summary>
        private readonly Type[] argClasses;
        #endregion

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
        /// <param name="methodInfo">The CLR method info.</param>
        /// <param name="instanceClass">The type of the instance argument.  Null if static or constructor.</param>
        /// <param name="argClassTypes">A list of argument types.</param>
        /// <param name="numberOfArgs">The required number of arguments.</param>
        protected ClrProcedure(string className, string methodName, MethodInfo methodInfo, Type instanceClass, Type[] argClassTypes, int numberOfArgs) :
            base(className + "." + methodName, new ArgsInfo(numberOfArgs, numberOfArgs, false))
        {
            this.className = className;
            this.methodInfo = methodInfo;
            this.instanceClass = (methodInfo == null || methodInfo.IsStatic || methodInfo.IsConstructor) ? null : instanceClass;
            this.argClasses = argClassTypes;
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the name of the class containing the method to invoke.
        /// </summary>
        protected string ClassName
        {
            get { return this.className; }
        }

        /// <summary>
        /// Gets information about the CLR method to be called.
        /// </summary>
        protected MethodInfo MethodInfo
        {
            get { return this.methodInfo; }
        }

        /// <summary>
        /// Gets the type of the instance variable.
        /// </summary>
        protected Type InstanceClass
        {
            get { return this.instanceClass; }
        }

        /// <summary>
        /// Gets the types of the arguments.
        /// </summary>
        protected Type[] ArgClasses
        {
            get { return this.argClasses; }
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the clr procedure primitives.
        /// </summary>
        /// <param name="primEnv">The environment to define the primitives into.</param>
        internal static new void DefinePrimitives(PrimitiveEnvironment primEnv)
        {
            primEnv
                .DefinePrimitive(
                    "class", 
                    new[] { "(class <class-name>)" },
                    (args, env, caller) => ClrObject.New(Class(First(args).ToString())), 
                    new ArgsInfo(1, ArgType.String))
                .DefinePrimitive(
                    "new", 
                    new[] { "(new <class-name>)" },
                    (args, env, caller) => ClrObject.New(New(First(args).ToString())), 
                    new ArgsInfo(1, ArgType.String))
                .DefinePrimitive(
                    "new-clr-array", 
                    new[] { "(new-clr-array <class-name> <length>)" },
                    (args, env, caller) => ClrObject.New(NewArray(First(args).ToString(), Second(args))), 
                    new ArgsInfo(2, ArgType.String, ArgType.Number))
                .DefinePrimitive(
                    "clr->native", 
                    new[] { "(clr->native <obj>)" },
                    (args, env, caller) => ClrObject.FromClrObject(First(args)), 
                    new ArgsInfo(1, ArgType.Obj));
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Display the CLR proc name as a string.  
        /// </summary>
        /// <returns>The string form of the procedure.</returns>
        public override string ToString()
        {
            return string.Format("clr-procedure: {0}", this.ProcedureName);
        }
        #endregion

        #region Protected Methods
        /// <summary>
        /// Look up the method info.
        /// </summary>
        /// <param name="className">The class name of the class to get method info about</param>
        /// <param name="theMethodName">The method name</param>
        /// <param name="argClassList">The argument types.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The method info for the method.</returns>
        protected static MethodInfo GetMethodInfo(
            string className, 
            string theMethodName, 
            Type[] argClassList, 
            Evaluator caller)
        {
            // TODO use caller to get line number
            caller.FindLineNumberInCallStack();
            try
            {
                Type cls = className.ToClass();
                if (cls == null)
                {
                    ErrorHandlers.ClrError(string.Format("Can't find class: {0} at line {1}", className, caller.FindLineNumberInCallStack()));
                    return null;
                }

                MethodInfo info = cls.GetMethod(theMethodName, argClassList);
                if (info == null)
                {
                    ErrorHandlers.ClrError(string.Format("Can't find method: {0}:{1} at line {2}", className, theMethodName, caller.FindLineNumberInCallStack()));
                    return null;
                }

                return info;
            }
            catch (TypeLoadException)
            {
                ErrorHandlers.ClrError(string.Format("Bad class, can't load: {0} at line {1}", className, caller.FindLineNumberInCallStack()));
                return null;
            }
        }

        /// <summary>
        /// Take a list of ArgType or type name elements and create a corresponding 
        ///   List of ArgType.
        /// </summary>
        /// <param name="args">A list of ArgType or type name elements.  There may be more than this.</param>
        /// <param name="extra">Number of extra types to allocate.</param>
        /// <returns>An array of Types corresponding to the list.</returns>
        protected static Type[] ClassList(SchemeObject args, int extra = 0)
        {
            var array = new Type[ListLength(args) + extra];

            int i = 0;
            while (args is Pair)
            {
                array[i++] = Class(First(args));
                args = Rest(args);
            }

            return array;
        }

        /// <summary>
        /// Get the class of a scheme object.
        /// </summary>
        /// <param name="arg">The scheme object.</param>
        /// <returns>The object's class.</returns>
        protected static Type Class(SchemeObject arg)
        {
            return arg.ToClass();
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
        /// <param name="evaluatorName">The evaluator name, for the error message.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>An array of arguments for the method call.</returns>
        protected object[] ToArgList(SchemeObject args, object[] additionalArgs, string evaluatorName, Evaluator caller)
        {
            // This has been checked once already, in Procedure.CheckArgCount, but that included
            //  an instance argument.  Check again to protect the rest of this method.
            int additionalN = additionalArgs != null ? additionalArgs.Length : 0;
            int numArgs = ListLength(args) + additionalN;
            int expectedArgs = this.ArgClasses.Length;
            if (numArgs != expectedArgs)
            {
                this.ArgCountError(numArgs, expectedArgs, args, evaluatorName, caller);
            }

            var array = new object[numArgs];

            int a = 0;
            while (args is Pair)
            {
                array[a] = ClrObject.ToClrObject(First(args), this.ArgClasses[a]);
                a++;
                args = Rest(args);
            }

            for (int i = 0; i < additionalN; i++)
            {
                Debug.Assert(additionalArgs != null, "ClrProcedure: additionalArgs != null");
                array[a++] = additionalArgs[i];
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
            Type type = Class(className);
            if (type == null)
            {
                ErrorHandlers.ClrError("ArgType cannot be found: " + className);
                return null;
            }

            return type.Assembly.CreateInstance(type.FullName);
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
            return Array.CreateInstance(Class(className), Number.AsInt(length));
        }
        #endregion
    }
}