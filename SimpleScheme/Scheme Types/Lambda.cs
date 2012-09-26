// <copyright file="Lambda.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

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
        public Lambda(SchemeObject formalParameters, SchemeObject body, Environment env) :
            base(null, new ArgsInfo(CountFormals(formalParameters)))
        {
            Contract.Requires(formalParameters != null);
            Contract.Requires(body != null);
            Contract.Requires(env != null);
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
            get
            {
                Contract.Ensures(Contract.Result<Environment>() != null);
                return this.env;
            }
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Display the lambda as a string.  
        /// Displays the formal parameters and the body, as it has been processed by the reader.
        /// </summary>
        /// <returns>The string form of the lambda.</returns>
        public override string ToString()
        {
            return this.ToString("lambda");
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Evaluate a lambda expression
        /// </summary>
        /// <param name="args">The lambda args.</param>
        /// <param name="env">The execution environment.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The lambda representing the expression.</returns>
        internal static Evaluator Call(SchemeObject args, Environment env, Evaluator caller)
        {
            Contract.Requires(args != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            caller.ReturnedExpr = new Lambda(First(args), Rest(args), env);
            return caller;
        }

        /// <summary>
        /// Actually executes the saved program
        /// Uses the given environment rather than creating a new one.
        /// The arguments have already been bound to the formal parameters.
        /// </summary>
        /// <param name="givenEnv">The environment to evaluate in.</param>
        /// <param name="returnTo">The calling evaluator.</param>
        /// <returns>The next evaluator to execute.</returns>
        internal Evaluator ApplyWithGivenEnv(Environment givenEnv, Evaluator returnTo)
        {
            Contract.Requires(givenEnv != null);
            Contract.Requires(returnTo != null);
            return EvaluateSequence.Call(this.body, givenEnv, returnTo);
        }

        /// <summary>
        /// Actually executes the saved program, with the given arguments bound with the 
        ///   formal parameters of the lambda.
        /// The outer environment is supplied from the Lambda itself.
        /// </summary>
        /// <param name="args">The values to be matched with the variable names.</param>
        /// <param name="returnTo">The evaluator to return to.  This can be different from returnTo if this is the last step in evaluation</param>
        /// <returns>The next evaluator to execute.</returns>
        internal override Evaluator Apply(SchemeObject args, Evaluator returnTo)
        {
#if Check
            this.CheckArgCount(ListLength(args), args, "Lambda");
#endif
            return this.ApplyWithGivenEnv(new Environment(this.Env, this.formalParameters, args), returnTo);
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
            Contract.Requires(tag != null);

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
            Contract.Ensures(Contract.Result<int[]>() != null);
            Contract.Ensures(Contract.Result<int[]>().Length == 2);
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

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.env != null);
            Contract.Invariant(this.body != null);
            Contract.Invariant(this.formalParameters != null);
        }
        #endregion
    }
}