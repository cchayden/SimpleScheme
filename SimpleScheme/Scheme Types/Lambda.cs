// <copyright file="Lambda.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
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
    internal class Lambda : Procedure
    {
        #region Fields
        /// <summary>
        /// A list of variable names, to be matched with values later.
        /// </summary>
        private readonly SchemeObject formalParameters;

        /// <summary>
        /// The program to execute.
        /// </summary>
        private readonly SchemeObject body;

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
        protected Lambda(SchemeObject formalParameters, SchemeObject body, Environment env) :
            base(null, new ArgsInfo(CountFormals(formalParameters)))
        {
            this.formalParameters = formalParameters;
            this.env = env;
            this.body = body;
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the environment in which to execute.
        /// </summary>
        internal Environment Env
        {
            get { return this.env; }
        }
        #endregion

        #region New
        /// <summary>
        /// Initializes a new instance of the Lambda class.
        /// </summary>
        /// <param name="formalParameters">A list of variable names, to be matched with 
        ///    values given later.</param>
        /// <param name="body">The program to execute.</param>
        /// <param name="env">The environment in which to execute it.</param>
        /// <returns>A Lambda.New.</returns>
        public static Lambda New(SchemeObject formalParameters, SchemeObject body, Environment env)
        {
            return new Lambda(formalParameters, body, env);
        }
        #endregion

        /// <summary>
        /// Evaluate a lambda expression
        /// </summary>
        /// <param name="args">The lambda args.</param>
        /// <param name="env">The execution environment.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The lambda representing the expression.</returns>
        internal static Evaluator Call(SchemeObject args, Environment env, Evaluator caller)
        {
            caller.ReturnedExpr = New(First(args), Rest(args), env);
            return caller;
        }

        #region Public Methods
        /// <summary>
        /// Display the lambda as a string.  
        /// Displays the formal parameters and the body, as it has been processed by the reader.
        /// </summary>
        /// <returns>The string form of the lambda.</returns>
        public override string ToString()
        {
            // TODO display formals??
            return this.ToString("lambda");
        }

        /// <summary>
        /// Actually executes the saved program
        /// Uses the given environment rather than creating a new one.
        /// The arguments have already been bound to the formal parameters.
        /// If there is only one expression in the body, just evaluate it, otherwise evaluate the sequence.
        /// </summary>
        /// <param name="givenEnv">The environment to evaluate in.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next evaluator to execute.</returns>
        internal Evaluator ApplyWithtEnv(Environment givenEnv, Evaluator caller)
        {
            return Rest(this.body) is EmptyList ? 
                EvaluateExpression.Call(First(this.body), givenEnv, caller) : 
                EvaluateSequence.Call(this.body, givenEnv, caller);
        }

        /// <summary>
        /// Actually executes the saved program, with the given arguments bound with the 
        ///   formal parameters of the lambda.
        /// Creates a new environment, linked to the environment of the lambda itself (which should be the lexical parent).
        /// </summary>
        /// <param name="args">The values to be matched with the variable names.</param>
        /// <param name="useEnv">Environment in which to apply.  If null, use the lambda's environment.</param>
        /// <param name="returnTo">The evaluator to return to.  This can be different from caller if this is the last step in evaluation</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The next evaluator to execute.</returns>
        internal override Evaluator Apply(SchemeObject args, Environment useEnv, Evaluator returnTo, Evaluator caller)
        {
#if Check
            this.CheckArgCount(ListLength(args), args, "Lambda", caller);
#endif
            useEnv = useEnv ?? this.Env;
            return this.ApplyWithtEnv(new Environment(this.formalParameters, args, useEnv), returnTo);
        }
        #endregion

        #region Private and Protected Methods
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

            return string.Format("({0} {1} {2})", tag, this.formalParameters, bodyStr);
        }

        /// <summary>
        /// Extract the min amd max number of args allowed when calling the lambda.
        /// </summary>
        /// <param name="vars">The formal parameter list.</param>
        /// <returns>The min and max argument counts.</returns>
        private static int[] CountFormals(SchemeObject vars)
        {
            const int MaxInt = int.MaxValue;
            if (vars is Symbol)
            {
                // variable: any number of arguments is permitted
                return new[] { 0, MaxInt };
            }

            int count = 0;
            while (!(vars is EmptyList))
            {
                // (var1 ... . varN): last arg gets all left over values
                if (!(vars is Pair))
                {
                    return new[] { count, MaxInt };
                }

                vars = Rest(vars);
                count++;
            }

            // it was a list -- the exact count is required
            return new[] { count, count };
        }
        #endregion
    }
}