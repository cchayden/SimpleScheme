// <copyright file="ClrProcedure.cs" company="Charles Hayden">
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
    /// Executes a function provided in the CLR.
    /// This class is immutable.  All attributes are set in constructors, either in this
    ///   class or in its subclasses.
    /// </summary>
    public abstract class ClrProcedure : Procedure
    {
        #region Constants
        /// <summary>
        /// The printable name of the clr procedure type.
        /// </summary>
        public new const string Name = "clr procedure";
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
        protected ClrProcedure(Obj className, Obj methodName) :
            base(0, 0)
        {
            this.ClassName = Printer.AsString(className, false);
            this.MethodName = Printer.AsString(methodName, false);
            this.ProcedureName = this.ClassName + "." + this.MethodName;
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
        /// Gets or sets information about the CLR method to be called.
        /// </summary>
        protected MethodInfo MethodInfo { get; set; }

        /// <summary>
        /// Gets or sets the types of the arguments.
        /// </summary>
        protected List<Type> ArgClasses { get; set; }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Tests whether to given object is a CLR procedure.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a CLR procedure.</returns>
        public static new bool Is(Obj obj)
        {
            return obj is ClrProcedure;
        }

        /// <summary>
        /// Convert object to clr procedure.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as a clr procedure.</returns>
        public static new ClrProcedure As(Obj obj)
        {
            if (Is(obj))
            {
                return (ClrProcedure)obj;
            }

            ErrorHandlers.TypeError(Name, obj);
            return null;
        }
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
                .DefinePrimitive("class", (args, caller) => Class(List.First(args)), 1)
                //// (new <class-name>)
                .DefinePrimitive("new", (args, caller) => New(List.First(args)), 1)
                //// (new-array <class-name> <length>)
                .DefinePrimitive("new-array", (args, caller) => NewArray(List.First(args), List.Second(args)), 2);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write the clr procedure to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public override void AsString(bool quoted, StringBuilder buf)
        {
            buf.Append(Name + ": ");
            buf.Append(this.ToString());
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
        /// Take a list of Type or type name elements and create a corresponding 
        ///   List of Type.
        /// </summary>
        /// <param name="args">A list of Type or type name elements.  There may be more than this.</param>
        /// <returns>An array of Types corresponding to the list.</returns>
        protected List<Type> ClassList(Obj args)
        {
            int n = List.Length(args);
            List<Type> array = new List<Type>(n);

            while (Pair.Is(args))
            {
                array.Add(TypePrimitives.ToClass(List.First(args)));
                args = List.Rest(args);
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
        /// <param name="additionalArgs">A list of the additional args, not supplied by the caller.</param>
        /// <returns>An array of arguments for the method call.</returns>
        protected object[] ToArgList(object args, object[] additionalArgs)
        {
            int n = List.Length(args);
            int additionalN = additionalArgs != null ? additionalArgs.Length : 0;
            int diff = n + additionalN - this.ArgClasses.Count;
            if (diff != 0)
            {
                ErrorHandlers.SemanticError(Math.Abs(diff) + 
                    " too " + (diff > 0 ? "many" : "few") + 
                    " args to " + ProcedureName);
            }

            object[] array = new object[n + additionalN];

            int i = 0;
            int a = 0;
            while (Pair.Is(args))
            {
                Obj elem = List.First(args);
                if (this.ArgClasses[i] == typeof(int))
                {
                    array[a++] = (int)Number.As(elem);
                }
                else if (this.ArgClasses[i] == typeof(string))
                {
                    array[a++] = Printer.AsString(elem, false);
                }
                else
                {
                    array[a++] = elem;
                }

                i++;
                args = List.Rest(args);
            }

            // required by style cop
            if (additionalArgs != null)    
            {
                for (i = 0; i < additionalN; i++)
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
        private static Obj Class(Obj className)
        {
            return TypePrimitives.ToClass(className);
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
        private static Obj New(Obj className)
        {
            Type type = TypePrimitives.ToClass(className);
            if (type == null)
            {
                ErrorHandlers.ClrError("Type cannot be found: " + Printer.AsString(className, false));
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
        private static Obj NewArray(Obj className, Obj length)
        {
            Type type = TypePrimitives.ToClass(className);
            return Array.CreateInstance(type, (int)Number.As(length));
        }
        #endregion
    }
}