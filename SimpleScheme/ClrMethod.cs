// <copyright file="ClrMethod.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Reflection;

    /// <summary>
    /// Executes a function provided in the CLR.
    /// </summary>
    public sealed class ClrMethod : Procedure
    {
        /// <summary>
        /// Information about the CLR method to be called.
        /// </summary>
        private readonly MethodInfo method;

        /// <summary>
        /// The types of the arguments.
        /// </summary>
        private readonly Type[] argClasses;

        /// <summary>
        /// True if the method is static.
        /// </summary>
        private readonly bool isStatic;

        /// <summary>
        /// Initializes a new instance of the ClrMethod class.
        /// This allows calls into CLR methods.
        /// The name of the method and the class that it is found in must be supplied.
        /// Also, a list of the types of the method arguments must be given.
        /// </summary>
        /// <param name="methodName">The name of the CLR method.</param>
        /// <param name="targetClassName">The name of the class containing the method.</param>
        /// <param name="argClassNames">A list of names of classes, the "types" of the arguments 
        /// of the method.</param>
        public ClrMethod(string methodName, object targetClassName, object argClassNames)
        {
            this.Name = targetClassName + "." + methodName;
            try
            {
                this.argClasses = ClassArray(argClassNames);
                this.method = ToClass(targetClassName).GetMethod(methodName, this.argClasses);
                this.isStatic = this.method.IsStatic;
            }
            catch (TypeLoadException)
            {
                Error("Bad class, can't get method: " + this.Name);
            }
            catch (MissingMethodException)
            {
                Error("Can't get method: " + this.Name);
            }
        }

        /// <summary>
        /// Create an instance of the given class.
        /// It is created using the default constructor.
        /// </summary>
        /// <param name="x">This is the class name.  It is either a type or the name 
        ///    of a type.</param>
        /// <returns>An instance of the class.</returns>
        public static object CreateInstance(object x)
        {
            Type type = ToClass(x);
            Assembly assembly = type.Assembly;
            return assembly.CreateInstance(type.FullName);
        }

        /// <summary>
        /// Apply the method to the given arguments.
        /// If the method is static, all arguments are passed to the method.
        /// Otherwise, the first argument is the class instance, and the rest are passed 
        ///    to the method.
        /// </summary>
        /// <param name="interpreter">The interpreter supplies the global environment.</param>
        /// <param name="parent">The calling evaluator.</param>
        /// <param name="args">Arguments to pass to the method.</param>
        /// <returns>The result of executing the method.</returns>
        public override object Apply(Scheme interpreter, Stepper parent, object args)
        {
            return this.isStatic ? 
                this.method.Invoke(null, this.ToArray(args)) : 
                this.method.Invoke(First(args), this.ToArray(Rest(args)));
        }

        /// <summary>
        /// Gets a CLR type from the given arg.
        /// Either it already holds a type, or else it holds a type name.
        /// If it is a name, then create the type from the name.
        /// </summary>
        /// <param name="arg">A Type or a type name.</param>
        /// <returns>The type corresponding to the name.</returns>
        private static Type ToClass(object arg)
        {
            if (arg is Type)
            {
                return (Type)arg;
            }

            var typeName = StringUtils.AsString(arg, false);
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
                default: return Type.GetType(typeName);
            }
        }

        /// <summary>
        /// Take a list of Type or type name elements and create a corresponding 
        ///   array of Type.
        /// </summary>
        /// <param name="args">A list of Type or type name elements.</param>
        /// <returns>An array of Type objects corresponding to the list.</returns>
        private static Type[] ClassArray(object args)
        {
            int n = Length(args);
            Type[] array = new Type[n];
            for (int i = 0; i < n; i++)
            {
                array[i] = ToClass(First(args));
                args = Rest(args);
            }

            return array;
        }

        /// <summary>
        /// Take a list of CLR method arguments and turn them into an array, suitable for
        ///   calling the method.
        /// Check to make sure the number and types of the arguments in the list match 
        ///    what is expected.
        /// </summary>
        /// <param name="args">A list of the method arguments.</param>
        /// <returns>An array of arguments for the method call.</returns>
        private object[] ToArray(object args)
        {
            int n = Length(args);
            int diff = n - this.argClasses.Length;
            if (diff != 0)
            {
                Error(Math.Abs(diff) + 
                    " too " + (diff > 0 ? "many" : "few") + 
                    " args to " + Name);
            }

            object[] array = new object[n];
            for (int i = 0; i < n && i < this.argClasses.Length; i++)
            {
                if (this.argClasses[i] == typeof(int))
                {
                    array[i] = (int)NumberUtils.Num(First(args));
                }
                else if (this.argClasses[i] == typeof(string))
                {
                    array[i] = StringUtils.AsString(First(args), false);
                }
                else
                {
                    array[i] = First(args);
                }

                args = Rest(args);
            }

            return array;
        }
    }
}