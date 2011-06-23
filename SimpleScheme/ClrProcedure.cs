﻿// <copyright file="ClrProcedure.cs" company="Charles Hayden">
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
                   (parent, args) =>
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
                    (parent, args) =>
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
                    1);
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
            switch (typeName)
            {
                case "void": return typeof(void);
                case "boolean": return typeof(bool);
                case "char": return typeof(char);
                case "byte": return typeof(byte);
                case "short": return typeof(short);
                case "int": return typeof(int);
                case "long": return typeof(long);
                case "float": return typeof(float);
                case "double": return typeof(double);
                case "string": return typeof(string);
                case "object": return typeof(object);
                default: return Type.GetType(typeName);
            }
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

            if (args is Pair)
            {
                foreach (var elem in (Pair)args)
                {
                    array.Add(ToClass(elem));
                }
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
        /// <returns>A (CLR) list of arguments for the method call.</returns>
        protected List<object> ToArgList(object args, object[] additionalArgs)
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

            List<object> array = new List<object>(n);

            if (args is Pair)
            {
                int i = 0;
                foreach (var elem in (Pair)args)
                {
                    if (this.ArgClasses[i] == typeof(int))
                    {
                        array.Add((int)Number.Num(elem));
                    }
                    else if (this.ArgClasses[i] == typeof(string))
                    {
                        array.Add(SchemeString.AsString(elem, false));
                    }
                    else
                    {
                        array.Add(elem);
                    }

                    i++;
                }
            }

            if (additionalArgs != null)
            {
                for (int i = 0; i < additionalN; i++)
                {
                    array.Add(additionalArgs[i]);
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
    }

    // TODO write unit tests
}