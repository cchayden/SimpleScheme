// <copyright file="EvaluateLet.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a let expression
    /// Bind the variables to values and then evaluate the expressions.
    /// </summary>
    //// <r4rs section="4.2.2">(let <bindings> <body>)</r4rs>
    //// <r4rs section="4.2.4">(let <variable> <bindings> <body>)</r4rs>
    //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
    //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
    public sealed class EvaluateLet : Evaluator
    {
        #region Fields

        /// <summary>
        /// The symbol "let"
        /// </summary>
        public static readonly Symbol LetSym = "let";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-let");

        /// <summary>
        /// Name, for named let.
        /// </summary>
        private readonly Symbol name;

        /// <summary>
        /// The body of the let.
        /// </summary>
        private readonly SchemeObject body;

        /// <summary>
        /// The list of variables to bind.
        /// </summary>
        private readonly SchemeObject vars;

        /// <summary>
        /// This list of initial expressions.
        /// </summary>
        private readonly SchemeObject inits;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateLet class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="name">The name of a named let.</param>
        /// <param name="body">The let body.</param>
        /// <param name="vars">The variables to bind.</param>
        /// <param name="inits">The initial values of the variables.</param>
        private EvaluateLet(SchemeObject expr, Environment env, Evaluator caller, Symbol name, SchemeObject body, SchemeObject vars, SchemeObject inits)
            : base(expr, env, caller, counter)
        {
            this.name = name;
            this.body = body;
            this.vars = vars;
            this.inits = inits;
            this.ContinueAt(InitialStep);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Call let evaluator.
        /// Start by checking the number of arguments.
        /// Then test for named let.  
        /// Grab bindings, body, and optionally name.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The let evaluator.</returns>
        public static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            if (expr is EmptyList)
            {
                ErrorHandlers.SemanticError("No arguments for let", null);
                return caller.UpdateReturnValue(Undefined.Instance);
            }

            if (!(expr is Pair))
            {
                ErrorHandlers.SemanticError("Bad arg list for let: " + expr, null);
                return caller.UpdateReturnValue(Undefined.Instance);
            }

            Symbol name = null;
            SchemeObject bindings;
            SchemeObject body;
            if (First(expr) is Symbol)
            {
                // named let
                name = (Symbol)First(expr);   
                bindings = Second(expr);
                body = Rest(Rest(expr));
            }
            else
            {
                bindings = First(expr);
                body = Rest(expr);
            }

            if (body is EmptyList)
            {
                return caller.UpdateReturnValue(Undefined.Instance);
            }

            SchemeObject vars = MapFun(First, MakeList(bindings));
            SchemeObject inits = MapFun(Second, MakeList(bindings));
            return new EvaluateLet(expr, env, caller, name, body, vars, inits);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Convert the let to a corresponding lambda.
        /// For a normal let, evaluate this lambda.
        /// For a named let, construct the additional lambda to bind to the name.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Continues by evaluating the constructed lambda.</returns>
        private static Evaluator InitialStep(Evaluator s)
        {
            var step = (EvaluateLet)s;
            if (step.name == null)
            {
                // regular let -- create a lambda for the body, bind inits to it, and apply it
                return EvaluateProc.Call(Lambda.New(step.vars, step.body, s.Env), step.inits, s.Env, s.Caller);
            }

            // named let -- eval the inits in the outer environment
            return EvaluateList.Call(step.inits, s.Env, s.ContinueAt(ApplyNamedLetStep));
        }

        /// <summary>
        /// Apply the named let.
        /// Define a lambda with the bound vars and the body, and put it in the environment
        ///   of the lambda.
        /// Then apply the lambda.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next evaluator to execute.</returns>
        private static Evaluator ApplyNamedLetStep(Evaluator s)
        {
            var step = (EvaluateLet)s;
            Lambda fn = Lambda.New(step.vars, step.body, s.Env);
            fn.Env.Define(step.name, fn);   
            return fn.Apply(EnsureSchemeObject(s.ReturnedExpr), s.Caller);
        }
        #endregion
    }
}