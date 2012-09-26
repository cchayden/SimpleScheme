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
        public new const string Name = "clr-procedure";
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
            this.SetName(this.ClassName + "." + this.MethodName);
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

        #region Public Static Methods
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
                    Symbol.New("class"), 
                    (args, caller) => Class(args.First()), 
                    1, 
                    Primitive.ValueType.String)
                //// (new <class-name>)
                .DefinePrimitive(
                    Symbol.New("new"), 
                    (args, caller) => New(args.First()), 
                    1, 
                    Primitive.ValueType.String)
                //// (new-array <class-name> <length>)
                .DefinePrimitive(
                    Symbol.New("new-array"), 
                    (args, caller) => NewArray(args.First(), args.Second()), 
                    2, 
                    Primitive.ValueType.String, 
                    Primitive.ValueType.Number);
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
        /// Look up the method info.
        /// </summary>
        /// <param name="theMethodName">The method name</param>
        /// <param name="argClassList">The argument types.</param>
        /// <returns>The method info for the method.</returns>
        protected MethodInfo GetMethodInfo(string theMethodName, List<Type> argClassList)
        {
            try
            {
                Type cls = TypePrimitives.ToClass(this.ClassName);
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
        protected List<Type> ClassList(Obj args)
        {
            int n = args.ListLength();
            var array = new List<Type>(n);

            while (args.IsPair())
            {
                array.Add(TypePrimitives.ToClass(args.First()));
                args = args.Rest();
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
            int n = args.ListLength();
            int additionalN = additionalArgs != null ? additionalArgs.Length : 0;
            int diff = n + additionalN - this.ArgClasses.Count;
            if (diff != 0)
            {
                ErrorHandlers.SemanticError(Math.Abs(diff) + 
                    " too " + (diff > 0 ? "many" : "few") + 
                    " args to " + this.ProcedureName);
            }

            var array = new object[n + additionalN];

            int i = 0;
            int a = 0;
            while (args.IsPair())
            {
                Obj elem = args.First();
                if (this.ArgClasses[i] == typeof(int))
                {
                    array[a++] = elem.AsInt();
                }
                else if (this.ArgClasses[i] == typeof(string))
                {
                    array[a++] = Printer.AsString(elem, false);
                }
                else if (this.ArgClasses[i] == typeof(bool))
                {
                    array[a++] = elem.AsBoolean();
                }
                else if (this.ArgClasses[i] == typeof(double))
                {
                    array[a++] = elem.AsDouble();
                }
                else if (this.ArgClasses[i] == typeof(float))
                {
                    array[a++] = elem.AsFloat();
                }
                else if (this.ArgClasses[i] == typeof(short))
                {
                    array[a++] = elem.AsShort();
                }
                else if (this.ArgClasses[i] == typeof(byte))
                {
                    array[a++] = elem.AsByte();
                }
                else if (this.ArgClasses[i] == typeof(char))
                {
                    array[a++] = elem.AsChar();
                }
                else if (this.ArgClasses[i] == typeof(System.IO.TextReader))
                {
                    array[a++] = elem.AsTextReader();
                }
                else if (this.ArgClasses[i] == typeof(System.IO.TextWriter))
                {
                    array[a++] = elem.AsTextWriter();
                }
                else if (this.ArgClasses[i] == typeof(char[]))
                {
                    array[a++] = Printer.AsString(elem, false).ToCharArray();
                }
                else
                {
                    array[a++] = elem;
                }

                i++;
                args = args.Rest();
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
                ErrorHandlers.ClrError("ValueType cannot be found: " + Printer.AsString(className, false));
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
            return Array.CreateInstance(type, length.AsInt());
        }
        #endregion
    }

    #region Extension Class
    /// <summary>
    /// Extensions for ClrProcedure
    /// </summary>
    public static class ClrProcedureExtensions
    {
        /// <summary>
        /// Tests whether to given object is a CLR procedure.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a CLR procedure.</returns>
        public static bool IsClrProcedure(this Obj obj)
        {
            return obj is ClrProcedure;
        }

        /// <summary>
        /// Convert object to clr procedure.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as a clr procedure.</returns>
        public static ClrProcedure AsClrProcedure(this Obj obj)
        {
            if (obj is ClrProcedure)
            {
                return (ClrProcedure)obj;
            }

            ErrorHandlers.TypeError(ClrProcedure.Name, obj);
            return null;
        }
    }
    #endregion
}