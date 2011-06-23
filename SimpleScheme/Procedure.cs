// <copyright file="Procedure.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// A procedure is executable. 
    /// It supports an Apply method.
    /// Closures, Continuations, CLR methods, and primitives are examples of Procedures.
    /// </summary>
    public abstract class Procedure : SchemeUtils
    {
        /// <summary>
        /// The default name of a procedure.
        /// </summary>
        private const string AnonymousProc = "anonymous procedure";

        /// <summary>
        /// Initializes a new instance of the Procedure class.
        /// Sets the name to the default.
        /// </summary>
        protected Procedure()
        {
            this.Name = AnonymousProc;
        }

        /// <summary>
        /// Gets or sets all Procedures have a name.  It can be set only by the subclass.
        /// </summary>
        public string Name { get; protected set; }

        /// <summary>
        /// Convert the given object to a procedure.
        /// It should be one already: if not, throw an error.
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The object as a procedure.</returns>
        public static Procedure Proc(object x)
        {
            if (x is Procedure)
            {
                return (Procedure)x;
            }

            return Proc(Error("Not a procedure: " + Stringify(x)));
        }

        /// <summary>
        /// Assign the procedure name.  If the name is still the default, assign it 
        ///    the name given in the argument.
        /// </summary>
        /// <param name="name">The name to assign it.</param>
        public void SetName(string name)
        {
            if (this.Name == AnonymousProc)
            {
                this.Name = name;
            }
        }

        /// <summary>
        /// The string form of a proc is its name in curly brackets.
        /// </summary>
        /// <returns>The name of the proc.</returns>
        public override string ToString()
        {
            return "{" + this.Name + "}";
        }

        /// <summary>
        /// All subclasses have to be able to apply the procedure to arguments.
        /// </summary>
        /// <param name="interpreter">The interpreter supplies the context.</param>
        /// <param name="args">The arguments to the procedure, which have 
        ///    been evaluated.</param>
        /// <returns>The result of applying the procedure to the arguments.</returns>
        public abstract object Apply(Scheme interpreter, object args);
    }
}