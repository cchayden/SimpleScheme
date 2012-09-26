// <copyright file="Lambda.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// A lambda stores the environment and a program to run.
    /// It can be executed later, through Apply.
    /// The Macro class also derives from this.
    /// This is effectively immutable.
    /// </summary>
    //// <r4rs section="4.1.4">(lambda <formals> <body>)</r4rs>
    //// <r4rs section="4.1.4">body: expression ...)</r4rs>
    //// <r4rs section="4.1.4">formals: (<variable1> ...)</r4rs>
    //// <r4rs section="4.1.4">formals: <variable></r4rs>
    //// <r4rs section="4.1.4">formals: (<variable 1> ... <variable n-1> . <variable n>)</r4rs>
#if OLD
    public class Lambda : Procedure, Cleanable
#else
    public class Lambda : Procedure
#endif
    {
        #region Constants
        /// <summary>
        /// The printable name of the lambda type.
        /// </summary>
        public new const string Name = "lambda";
        #endregion

        #region Fields
        /// <summary>
        /// A list of variable names, to be matched with values later.
        /// </summary>
        private readonly Obj formalParameters;

        /// <summary>
        /// The program to execute.
        /// </summary>
        private readonly Obj body;

        /// <summary>
        /// The environment in which to execute.
        /// </summary>
        private readonly Environment env;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Lambda class.
        /// </summary>
        /// <param name="formalParameters">A list of variable names, to be matched with 
        ///    values given later.</param>
        /// <param name="body">The program to execute.</param>
        /// <param name="env">The environment in which to execute it.</param>
        protected Lambda(Obj formalParameters, Obj body, Environment env) :
            base(0, 0)
        {
            this.formalParameters = formalParameters;
            this.env = env;
            this.body = body;
            this.ProcessFormals();
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the environment in which to execute.
        /// </summary>
        public Environment Env
        {
            get { return this.env; }
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Create a new Lambda.
        /// </summary>
        /// <param name="formalParameters">A list of variable names, to be matched with 
        ///    values given later.</param>
        /// <param name="body">The program to execute.</param>
        /// <param name="env">The environment in which to execute it.</param>
        /// <returns>A new Lambda.</returns>
        public static Lambda New(Obj formalParameters, Obj body, Environment env)
        {
            return new Lambda(formalParameters, body, env);
        }

        #region Public Methods
        /// <summary>
        /// Write the lambda to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public new void PrintString(bool quoted, StringBuilder buf)
        {
            buf.Append(this.ToString());
        }

        /// <summary>
        /// Display the lambda as a string.  
        /// Displays the formal parameters and the body, as it has been processed by the reader.
        /// </summary>
        /// <returns>The string form of the lambda.</returns>
        public override string ToString()
        {
            return this.ToString(Name);
        }

#if OLD
        /// <summary>
        /// Clean the body.
        /// This may need to be cleaned after executing in a different environment.
        /// Used when executing continuations.
        /// </summary>
        public void Clean()
        {
            Cleaner.Clean(this.body);
        }
#endif
        #endregion

        /// <summary>
        /// Actually executes the saved program
        /// Uses the given environment rather than creating a new one.
        /// The arguments have already been bound to the formal parameters.
        /// If there is only one expression in the body, just evaluate it, otherwise evaluate the sequence.
        /// </summary>
        /// <param name="givenEnv">The environment to evaluate in.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next evaluator to execute.</returns>
        public Evaluator ApplyWithtEnv(Environment givenEnv, Evaluator caller)
        {
            return this.body.Rest().IsEmptyList() ? 
                EvaluateExpression.Call(this.body.First(), givenEnv, caller) : 
                EvaluateSequence.Call(this.body, givenEnv, caller);
        }

        /// <summary>
        /// Actually executes the saved program, with the given arguments bound with the 
        ///   formal parameters of the lambda.
        /// Creates a new environment, linked to the environment of the lambda itself (which should be the lexical parent).
        /// </summary>
        /// <param name="args">The values to be matched with the variable names.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next evaluator to execute.</returns>
        public override Evaluator Apply(Obj args, Evaluator caller)
        {
            CheckArgs(args, "Lambda");
            return this.ApplyWithtEnv(new Environment(this.formalParameters, args, this.Env), caller);
        }

        /// <summary>
        /// Common printing logic for lambdas.
        /// </summary>
        /// <param name="tag">The function name.</param>
        /// <returns>String representing the lambda.</returns>
        protected string ToString(string tag)
        {
            // trim enclosing parentheses off body, to match the definition
            string bodyStr = this.body.ToString();
            if (bodyStr.StartsWith("(") && bodyStr.EndsWith(")"))
            {
                bodyStr = bodyStr.Substring(1, bodyStr.Length - 2).Trim();
            }

            return String.Format("({0} {1} {2})", tag, this.formalParameters, bodyStr);
        }

        /// <summary>
        /// Extract the min amd max number of args allowed when calling the lambda.
        /// </summary>
        private void ProcessFormals()
        {
            const int MaxInt = int.MaxValue;
            int count = 0;
            Obj vars = this.formalParameters;
            while (true)
            {
                if (vars.IsEmptyList())
                {
                    this.SetMinMax(count);
                    return;
                }

                if (!vars.IsPair())
                {
                    this.SetMinMax(count, vars.IsSymbol() ? MaxInt : count);
                    return;
                }

                vars = vars.Rest();
                count++;
            }
        }
        #endregion
    }

    /// <summary>
    /// Extensions for Lambda
    /// </summary>
    public static class LambdaExtensions
    {
        /// <summary>
        /// Tests whether to given object is a scheme lambda.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme lambda.</returns>
        public static bool IsLambda(this Obj obj)
        {
            return obj is Lambda;
        }

        /// <summary>
        /// Convert object to lambda.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as a lambda.</returns>
        public static Lambda AsLambda(this Obj obj)
        {
            return (Lambda)obj;
        }
    }
}