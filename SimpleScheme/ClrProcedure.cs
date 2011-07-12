// <copyright file="ClrProcedure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Reflection;
    using Obj = System.Object;

    /// <summary>
    /// Executes a function provided in the CLR.
    /// </summary>
    internal abstract class ClrProcedure : Procedure
    {
        #region Constants
        /// <summary>
        /// The primitive types that can be used as args.
        /// </summary>
        private static readonly string[][] primitiveTypes = 
        {
            new[] { "boolean", "System.Boolean" },
            new[] { "char", "System.Char" }, 
            new[] { "string", "System.String" }, 
            new[] { "byte", "System.Byte" },
            new[] { "short", "System.Int16" }, 
            new[] { "int", "System.Int32" }, 
            new[] { "long", "System.Int64" }, 
            new[] { "float", "System.Single" }, 
            new[] { "double", "System.Double" }, 
        };
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the ClrProcedure class.
        /// This allows calls into CLR methods.
        /// The name of the method and the class that it is found in must be supplied.
        /// Also, a list of the types of the method arguments must be given.
        /// </summary>
        /// <param name="targetClassName">The name of the class containing the method.
        ///   of the method.</param>
        /// <param name="methodName">The name of the CLR method.</param>
        protected ClrProcedure(Obj targetClassName, Obj methodName)
        {
            this.ClassName = SchemeString.AsString(targetClassName, false);
            this.MethodName = SchemeString.AsString(methodName, false);
            this.Name = this.ClassName + "." + this.MethodName;
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets or sets the name of the class containing the method to invoke.
        /// </summary>
        protected string ClassName { get; set; }

        /// <summary>
        /// Gets or sets the name of the method.
        /// </summary>
        protected string MethodName { get; set; }

        /// <summary>
        /// Gets or sets information about the CLR method to be called.
        /// </summary>
        protected MethodInfo MethodInfo { get; set; }

        /// <summary>
        /// Gets or sets the types of the arguments.
        /// </summary>
        protected List<Type> ArgClasses { get; set; }
        #endregion

        #region Public Methods
        /// <summary>
        /// Display the CLR proc name as a string.  
        /// </summary>
        /// <returns>The string form of the continuation.</returns>
        public override string ToString()
        {
            return string.Format("ClrProcedure {0}", this.Name);
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the clr procedure primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        internal static new void DefinePrimitives(Environment env)
        {
            env
                //// (class <class-name>)
                .DefinePrimitive("class", (args, caller) => Class(First(args)), 1)
                //// (new <class-name>)
                .DefinePrimitive("new", (args, caller) => New(First(args)), 1)
                //// (new-array <class-name> <length>)
                .DefinePrimitive("new-array", (args, caller) => NewArray(First(args), Second(args)), 2);
        }
        #endregion

        #region Protected Static Methods
        /// <summary>
        /// Gets a CLR type from the given arg.
        /// Either it already holds a type, or else it holds a type name.
        /// If it is a name, then create the type from the name.
        /// </summary>
        /// <param name="arg">A Type or a type name.</param>
        /// <returns>The type corresponding to the name.</returns>
        protected static Type ToClass(Obj arg)
        {
            if (arg is Type)
            {
                return (Type)arg;
            }

            var typeName = SchemeString.AsString(arg, false);
            foreach (var type in primitiveTypes)
            {
                string abbrev = type[0];
                string full = type[1];
                if (typeName == abbrev)
                {
                    return Type.GetType(full);
                }

                if (typeName == abbrev + "[]")
                {
                    return Type.GetType(full + "[]");
                }
            }

            return typeName == "void" ? typeof(void) : Type.GetType(typeName);
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
            int n = Length(args);
            List<Type> array = new List<Type>(n);

            while (args is Pair)
            {
                array.Add(ToClass(First(args)));
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
        /// <param name="additionalArgs">A list of the additional args, not supplied by the caller.</param>
        /// <returns>An array of arguments for the method call.</returns>
        protected object[] ToArgList(object args, object[] additionalArgs)
        {
            int n = Length(args);
            int additionalN = additionalArgs != null ? additionalArgs.Length : 0;
            int diff = n + additionalN - this.ArgClasses.Count;
            if (diff != 0)
            {
                ErrorHandlers.SemanticError(Math.Abs(diff) + 
                    " too " + (diff > 0 ? "many" : "few") + 
                    " args to " + Name);
            }

            object[] array = new object[n + additionalN];

            int i = 0;
            int a = 0;
            while (args is Pair)
            {
                Obj elem = First(args);
                if (this.ArgClasses[i] == typeof(int))
                {
                    array[a++] = (int)Number.Num(elem);
                }
                else if (this.ArgClasses[i] == typeof(string))
                {
                    array[a++] = SchemeString.AsString(elem, false);
                }
                else
                {
                    array[a++] = elem;
                }

                i++;
                args = Rest(args);
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
        /// </summary>
        /// <param name="className">The class name.  May have to be assembly-qualified.</param>
        /// <returns>The type object.</returns>
        private static Obj Class(Obj className)
        {
            try
            {
                return Type.GetType(SchemeString.AsString(className, false));
            }
            catch (TypeLoadException)
            {
            }

            return Undefined.Instance;
        }

        /// <summary>
        /// Create a new object instance.
        /// This requires the class to have a default constructor.
        /// The class name must be assembly-qualified, unless it is in a standard assembly.
        /// </summary>
        /// <param name="className">The class name.</param>
        /// <returns>A new instance of the class.</returns>
        private static Obj New(Obj className)
        {
            try
            {
                return CreateInstance(className);
            }
            catch (ArgumentNullException)
            {
            }
            catch (ArgumentException)
            {
            }
            catch (BadImageFormatException)
            {
            }
            catch (MissingMethodException)
            {
            }
            catch (FileLoadException)
            {
            }
            catch (FileNotFoundException)
            {
            }
            catch (TargetInvocationException)
            {
            }

            return Undefined.Instance;
        }

        /// <summary>
        /// Create an array of objects.
        /// </summary>
        /// <param name="className">The class name of the array elements.</param>
        /// <param name="len">The array length.</param>
        /// <returns>An array of the given length.</returns>
        private static Obj NewArray(Obj className, Obj len)
        {
            try
            {
                return CreateArrayInstance(className, len);
            }
            catch (ArgumentNullException)
            {
            }
            catch (ArgumentException)
            {
            }
            catch (BadImageFormatException)
            {
            }
            catch (MissingMethodException)
            {
            }
            catch (FileLoadException)
            {
            }
            catch (FileNotFoundException)
            {
            }
            catch (TargetInvocationException)
            {
            }

            return Undefined.Instance;
        }

        /// <summary>
        /// Create an instance of the given class.
        /// It is created using the default constructor.
        /// </summary>
        /// <param name="className">This is the class name.  It is either a type or the name 
        ///    of a type.</param>
        /// <returns>An instance of the class.</returns>
        private static object CreateInstance(object className)
        {
            Type type = ToClass(className);
            if (type == null)
            {
                return ErrorHandlers.ClrError("Type cannot be found: " + SchemeString.AsString(className, false));
            }

            Assembly assembly = type.Assembly;
            return assembly.CreateInstance(type.FullName);
        }

        /// <summary>
        /// Create an array of instances of the given class.
        /// </summary>
        /// <param name="className">This is the class name.  It is either a type or the name 
        ///    of a type.</param>
        /// <param name="length">The array length.</param>
        /// <returns>An array of the class.</returns>
        private static object CreateArrayInstance(object className, object length)
        {
            Type type = ToClass(className);
            int len = (int)Number.Num(length);
            return Array.CreateInstance(type, len);
        }
        #endregion
    }
}