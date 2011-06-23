// <copyright file="ClrProcedure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Reflection;

    /// <summary>
    /// Executes a function provided in the CLR.
    /// </summary>
    public abstract class ClrProcedure : Procedure
    {

        /// <summary>
        /// The primitive types that can be used as args.
        /// </summary>
        private static readonly string[][] primitiveTypes = {
                                                     new [] {"boolean", "System.Boolean"},
                                                     new [] {"char", "System.Char"}, 
                                                     new [] {"byte", "System.Byte"},
                                                     new [] {"short", "System.Int16"}, 
                                                     new [] {"int", "System.Int32"}, 
                                                     new [] {"long", "System.Int64"}, 
                                                     new [] {"float", "System.Single"}, 
                                                     new [] {"double", "System.Double"}, 
                                                     new [] {"string", "System.String"}, 
                                                     new [] {"object", "Object"}
                                                 };

        /// <summary>
        /// Initializes a new instance of the ClrProcedure class.
        /// This allows calls into CLR methods.
        /// The name of the method and the class that it is found in must be supplied.
        /// Also, a list of the types of the method arguments must be given.
        /// </summary>
        /// <param name="methodName">The name of the CLR method.</param>
        /// <param name="targetClassName">The name of the class containing the method.
        /// of the method.</param>
        protected ClrProcedure(string methodName, object targetClassName)
        {
            this.ClassName = SchemeString.AsString(targetClassName, false);
            this.Name = this.ClassName + "." + methodName;
        }

        /// <summary>
        /// Gets or sets the name of the class containing the method to invoke.
        /// </summary>
        protected string ClassName { get; set; }

        /// <summary>
        /// Gets or sets information about the CLR method to be called.
        /// </summary>
        protected MethodInfo MethodInfo { get; set; }

        /// <summary>
        /// Gets or sets the types of the arguments.
        /// </summary>
        protected List<Type> ArgClasses { get; set; }

        /// <summary>
        /// Define the clr procedure primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static new void DefinePrimitives(Environment env)
        {
            env
                .DefinePrimitive(
                   "class", 
                   (caller, args) =>
                        {
                            try
                            {
                                return Type.GetType(SchemeString.AsString(First(args), false));
                            }
                            catch (TypeLoadException)
                            {
                            }

                            return SchemeBoolean.False;
                        }, 
                     1)
                .DefinePrimitive(
                    "new",
                    (caller, args) =>
                        {
                            try
                            {
                                return CreateInstance(First(args));
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

                            return SchemeBoolean.False;
                        },
                    1)
                .DefinePrimitive(
                    "new-array",
                    (caller, args) =>
                        {
                            try
                            {
                                return CreateArrayInstance(First(args), Second(args));
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

                            return SchemeBoolean.False;
                        },
                    2);
        }

        /// <summary>
        /// Display the CLR proc name as a string.  
        /// </summary>
        /// <returns>The string form of the continuation.</returns>
        public override string ToString()
        {
            return string.Format("ClrProcedure {0}", this.Name);
        }

        /// <summary>
        /// Gets a CLR type from the given arg.
        /// Either it already holds a type, or else it holds a type name.
        /// If it is a name, then create the type from the name.
        /// </summary>
        /// <param name="arg">A Type or a type name.</param>
        /// <returns>The type corresponding to the name.</returns>
        protected static Type ToClass(object arg)
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

        /// <summary>
        /// Take a list of Type or type name elements and create a corresponding 
        ///   List of Type.
        /// </summary>
        /// <param name="args">A list of Type or type name elements.  There may be more than this.</param>
        /// <returns>An array of Type objects corresponding to the list.</returns>
        protected List<Type> ClassList(object args)
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
                ErrorHandlers.Error(Math.Abs(diff) + 
                    " too " + (diff > 0 ? "many" : "few") + 
                    " args to " + Name);
            }

            object[] array = new object[n + additionalN];

            int i = 0;
            int a = 0;
            while (args is Pair)
            {
                object elem = First(args);
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

            if (additionalArgs != null)    // required by style cop
            {
                for (i = 0; i < additionalN; i++)
                {
                    array[a++] = additionalArgs[i];
                }
            }

            return array;
        }

        /// <summary>
        /// Create an instance of the given class.
        /// It is created using the default constructor.
        /// </summary>
        /// <param name="x">This is the class name.  It is either a type or the name 
        ///    of a type.</param>
        /// <returns>An instance of the class.</returns>
        private static object CreateInstance(object x)
        {
            Type type = ToClass(x);
            Assembly assembly = type.Assembly;
            return assembly.CreateInstance(type.FullName);
        }

        /// <summary>
        /// Create an array of instances of the given class.
        /// </summary>
        /// <param name="x">This is the class name.  It is either a type or the name 
        ///    of a type.</param>
        /// <param name="length">The array length.</param>
        /// <returns>An array of the class.</returns>
        private static object CreateArrayInstance(object x, object length)
        {
            Type type = ToClass(x);
            int len = (int)Number.Num(length);
            return Array.CreateInstance(type, len);
        }
    }

    // TODO write unit tests
}