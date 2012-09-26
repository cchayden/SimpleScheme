// <copyright file="EvaluateLetStar.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a let* expression
    /// Bind the variables to values and then evaluate the expressions.
    /// </summary>
    //// <r4rs section="4.2.2">(let* <bindings> <body>)</r4rs>
    //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
    //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
    internal sealed class EvaluateLetStar : Evaluator
    {
        #region Fields
        /// <summary>
        /// The body of the let.
        /// </summary>
        private readonly SchemeObject body;

        /// <summary>
        /// The list of variables to bind.
        /// </summary>
        private SchemeObject vars;

        /// <summary>
        /// This list of initial expressions.
        /// </summary>
        private SchemeObject inits;

        /// <summary>
        /// Evaluated values of inits.
        /// </summary>
        private SchemeObject vals;

        /// <summary>
        /// The list of formal parameters to pass to the final lambda.
        /// This is built up from variable1, variable2, ...
        /// It is in the reverse order from the given list.
        /// </summary>
        private SchemeObject formals;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateLetStar class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="body">The let* body.</param>
        /// <param name="vars">The list of variables to bind.</param>
        /// <param name="inits">The initialization expressions.</param>
        /// <param name="formals">The list of parameters to pass to the lambda.</param>
        /// <param name="vals">Evaluated values of inits.</param>
        private EvaluateLetStar(SchemeObject expr, Environment env, Evaluator caller, SchemeObject body, SchemeObject vars, SchemeObject inits, SchemeObject formals, SchemeObject vals)
            : base(EvalInitStep, expr, env, caller)
        {
            this.body = body;
            this.vars = vars;
            this.inits = inits;
            this.formals = formals;
            this.vals = vals;
        }
        #endregion

        #region Call
        /// <summary>
        /// Call let* evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The let evaluator.</returns>
        internal static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            if (expr is EmptyList)
            {
                ErrorHandlers.SemanticError("No arguments arguments for let*", null);
            }

            if (!(expr is Pair))
            {
                ErrorHandlers.SemanticError("Bad arg list for let*: " + expr, null);
            }

            SchemeObject bindings = First(expr);
            SchemeObject body = Rest(expr);

            if (body is EmptyList)
            {
                caller.ReturnedExpr = Undefined.Instance;
                return caller;
            }

            SchemeObject vars = MapFun(First, bindings);
            SchemeObject inits = MapFun(Second, bindings);
            SchemeObject formals = EmptyList.Instance;
            SchemeObject vals = EmptyList.Instance;
            return new EvaluateLetStar(expr, env, caller, body, vars, inits, formals, vals);
        }
        #endregion

        #region steps
        /// <summary>
        /// Evaluate one of the inits.
        /// Do it in an environment made up of the previously evaluated inits bound to their vars.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next step.</returns>
        private static Evaluator EvalInitStep(Evaluator s)
        {
            var step = (EvaluateLetStar)s;
            if (step.inits is EmptyList)
            {
                s.Pc = ApplyProcStep;
                return s;
            }

            Lambda fun = Lambda.New(step.formals, MakeList(First(step.inits)), s.Env);
            s.Pc = BindVarToInitStep;
            return fun.Apply(step.vals, fun.Env, s, s);
        }

        /// <summary>
        /// Add var to list of formals, evaluation result to list of vals.
        /// Move down list of vars and inits.
        /// Go back and evaluate another init.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next step.</returns>
        private static Evaluator BindVarToInitStep(Evaluator s)
        {
            var step = (EvaluateLetStar)s;
            step.formals = Cons(First(step.vars), step.formals);
            step.vals = Cons(s.ReturnedExpr, step.vals);
            step.vars = Rest(step.vars);
            step.inits = Rest(step.inits);
            s.Pc = EvalInitStep;
            return s;
        }

        /// <summary>
        /// Inits evaluated and bound -- execute the proc.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Execution continues with evaluation of the body of the let.</returns>
        private static Evaluator ApplyProcStep(Evaluator s)
        {
            var step = (EvaluateLetStar)s;

            // apply the fun to the vals
            Lambda fun = Lambda.New(step.formals, step.body, s.Env);
            return fun.Apply(step.vals, fun.Env, s.Caller, s);
        }
        #endregion
    }
}