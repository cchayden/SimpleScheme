// <copyright file="Procedure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// A procedure is executable. 
    /// It supports an Apply method.
    /// Lambdas, Continuations, CLR methods, and primitives are examples of Procedures.
    /// </summary>
    public abstract class Procedure : IPrintable, ISchemeType
    {
        #region Constants
        /// <summary>
        /// The default name of a procedure.
        /// </summary>
        private const string AnonymousProc = "anonymous procedure";
        #endregion

        #region Fields

        /// <summary>
        /// The minimum number of args for the procedure.
        /// Set only in the constructor here and in subclasses.
        /// </summary>
        private int minArgs;

        /// <summary>
        /// The maximum number of args for the procedure.
        /// Set only in the constructor here and in subclasses.
        /// </summary>
        private int maxArgs;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Procedure class, setting min and max args.
        /// </summary>
        /// <param name="minArgs">The minimum number of args.</param>
        /// <param name="maxArgs">The maximum number of args.</param>
        protected Procedure(int minArgs, int maxArgs)
        {
            this.ProcedureName = AnonymousProc;
            this.minArgs = minArgs;
            this.maxArgs = maxArgs;
        }
        #endregion

        #region SchemeType Accessors
        /// <summary>
        /// Gets the name of the type.
        /// </summary>
        public virtual string TypeName
        {
            get { return TypePrimitives.ValueTypeName(TypePrimitives.ValueType.Proc); }
        }
        #endregion

        #region Accessors

        /// <summary>
        /// Gets all Procedures have a name.  It can be set only by the subclass.
        /// </summary>
        public string ProcedureName { get; private set; }

        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the procedure primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(PrimitiveEnvironment env)
        {
            const int MaxInt = int.MaxValue;
            env
                //// <r4rs section="6.9">(apply <proc> <args>)</r4rs>
                //// <r4rs section="6.9">(apply <proc> <arg1> ... <args>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("apply"), 
                        (args, caller) => args.First().AsProcedure().Apply(args.Rest().ListStar(), caller), 
                        2, 
                        MaxInt, 
                        TypePrimitives.ValueType.Proc, 
                        TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.9"> (call-with-current-continuation <proc>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("call-with-current-continuation"), 
                        (args, caller) => args.First().AsProcedure().CallCc(caller), 
                        1, 
                        TypePrimitives.ValueType.Proc)
                .DefinePrimitive(
                        Symbol.New("call/cc"), 
                        (args, caller) => args.First().AsProcedure().CallCc(caller), 
                        1, 
                        TypePrimitives.ValueType.Proc)

                //// <r4rs section="6.9">(force <promise>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("force"), 
                        (args, caller) => Force(args.First(), caller), 
                        1, 
                        TypePrimitives.ValueType.Proc)
                //// <r4rs section="6.9">(for-each <proc> <list1> <list2> ...)</r4rs>
                .DefinePrimitive(
                        Symbol.New("for-each"), 
                        (args, caller) => EvaluateMap.Call(args.First().AsProcedure(), args.Rest(), false, caller.Env, caller), 
                         1, 
                        MaxInt,
                        TypePrimitives.ValueType.Proc, 
                        TypePrimitives.ValueType.Pair)
                //// <r4rs section="6.9">(map <proc> <list1> <list2> ...)</r4rs>
                .DefinePrimitive(
                        Symbol.New("map"), 
                        (args, caller) => EvaluateMap.Call(args.First().AsProcedure(), args.Rest(), true, caller.Env, caller), 
                        1, 
                        MaxInt, 
                        TypePrimitives.ValueType.Proc, 
                        TypePrimitives.ValueType.PairOrEmpty)
                //// <r4rs section="6.9">(procedure? <obj>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("procedure?"), 
                        (args, caller) => SchemeBoolean.Truth(args.First().IsProcedure()), 
                        1, 
                        TypePrimitives.ValueType.Obj);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Identifies objects of this scheme type.
        /// </summary>
        /// <param name="obj">The object to test.</param>
        /// <returns>True if the object is this scheme type.</returns>
        public static bool Is(Obj obj)
        {
            return obj is Procedure;
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write the procedure to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public void PrintString(bool quoted, StringBuilder buf)
        {
            buf.Append(this.ToString());
        }

        /// <summary>
        /// Display the procedure as a string.
        /// Since there is nothing to show, at least give the type.
        /// </summary>
        /// <returns>The procedure type name.</returns>
        public override string ToString()
        {
            return "<procedure>";
        }

        /// <summary>
        /// All subclasses have to be able to apply the procedure to arguments.
        /// That is what it means to be a procedure.
        /// </summary>
        /// <param name="args">The arguments to the procedure, which may or may not have 
        ///   been evaluated.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next evaluator to run after the application.</returns>
        public abstract Evaluator Apply(Obj args, Evaluator caller);

        /// <summary>
        /// Evaluate the procedure.
        /// At this point, the args are NOT evaluated
        /// Macro is evaluated differently, and overrides this method.
        /// This is a primitive, a lambda, a continuation, or a CLR Procedure.
        /// Evaluate the arguments in the environment, then apply the function 
        ///    to the arguments.
        /// </summary>
        /// <param name="args">The arguments to the procedure.</param>
        /// <param name="env">The environment to use for the application.</param>
        /// <param name="caller">Return here when done.</param>
        /// <returns>The next evaluator to execute.</returns>
        public virtual Evaluator Evaluate(Obj args, Environment env, Evaluator caller)
        {
            return EvaluateProc.Call(this, args, env, caller);
        }

        /// <summary>
        /// Assign the procedure name.  If the name is still the default, assign it 
        ///    the name given in the argument.
        /// This is used to associate names with lambdas.  If a "define" is defining
        ///   a lambda, then the name field of the lambda is set as a side effect.
        /// Not all lambdas are defined however, so it may remain anonymous.
        /// </summary>
        /// <param name="name">The name to assign it.</param>
        public void SetName(string name)
        {
            if (this.ProcedureName == AnonymousProc)
            {
                this.ProcedureName = name;
            }
        }
        #endregion

        #region Protected Methods
        /// <summary>
        /// Set the min and max number of arguments accepted.
        /// </summary>
        /// <param name="minCount">The minimum number of args accepted.</param>
        /// <param name="maxCount">The maximum number of args accepted.</param>
        protected void SetMinMax(int minCount, int maxCount)
        {
            this.minArgs = minCount;
            this.maxArgs = maxCount;
        }

        /// <summary>
        /// Set the number of arguments accepted.
        /// </summary>
        /// <param name="count">The number of args accepted.</param>
        protected void SetMinMax(int count)
        {
            this.minArgs = count;
            this.maxArgs = count;
        }

        /// <summary>
        /// Check the number of args passed.
        /// </summary>
        /// <param name="args">The arguments passed to the procedure.</param>
        /// <param name="tagType">Name, for the error message.</param>
        /// <returns>The number of arguments passed to the procedure.</returns>
        protected int CheckArgs(Obj args, Type tagType)
        {
            int numArgs = args.ListLength();
            if (numArgs < this.minArgs || numArgs > this.maxArgs)
            {
                string tag = TypePrimitives.SchemeTypeName(tagType);
                string msg = numArgs < this.minArgs ? "few" : "many";
                ErrorHandlers.SemanticError(
                    string.Format(
                       @"""{0}"" too {1} args ({2}) for {3}: ""{4}""", 
                      tag, 
                      msg, 
                      numArgs, 
                      this.ProcedureName, 
                      args));
            }

            return numArgs;
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Force a promise.  The promise is a proc: apply it.
        /// </summary>
        /// <param name="promise">A proc that will produce the result.</param>
        /// <param name="caller">The caller.</param>
        /// <returns>The result of applying the proc.</returns>
        private static Obj Force(Obj promise, Evaluator caller)
        {
            return !promise.IsProcedure() ? promise : promise.AsProcedure().Apply(null, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Perform the call/cc primitive.
        /// Create a continuation that captures the caller's environment and returns to the caller.
        /// Then apply this procedure to it.
        /// Clean the lambda first, because it could have been previously executed in another environment.
        /// </summary>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>A function to continue the evaluation.</returns>
        private Obj CallCc(Evaluator caller)
        {
            return this.Apply(Continuation.New(caller).MakeList(), caller);
        }
        #endregion
    }

    #region Extension Class
    static class ProcedureExtension
    {
        /// <summary>
        /// Tests whether to given object is a procedure.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme procedure.</returns>
        public static bool IsProcedure(this Obj obj)
        {
            return Procedure.Is(obj);
        }

        /// <summary>
        /// Cast the given object to a procedure.
        /// </summary>
        /// <param name="x">The obj to test.</param>
        /// <returns>The procedure.</returns>
        public static Procedure AsProcedure(this Obj x)
        {
            if (Procedure.Is(x))
            {
                return (Procedure)x;
            }

            ErrorHandlers.TypeError(typeof(Procedure), x);
            return null;
        }
    }
    #endregion
}