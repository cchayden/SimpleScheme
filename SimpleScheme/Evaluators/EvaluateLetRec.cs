﻿// <copyright file="EvaluateLetRec.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a letrec expression
    /// Bind the variables to values and then evaluate the expressions.
    /// </summary>
    //// <r4rs section="4.2.2">(letrec<bindings> <body>)</r4rs>
    //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
    //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
    internal sealed class EvaluateLetRec : Evaluator
    {
        #region Fields
        /// <summary>
        /// The body of the let.
        /// </summary>
        private readonly SchemeObject body;

        /// <summary>
        /// The list of formal parameters to pass to the final lambda.
        /// This is variable1, variable2, ...
        /// </summary>
        private readonly SchemeObject formals;

        /// <summary>
        /// The list of variables to bind.
        /// </summary>
        private SchemeObject vars;

        /// <summary>
        /// This list of initial expressions.
        /// </summary>
        private SchemeObject inits;

        /// <summary>
        /// Accumulate the list of init vals here for later assignment.
        /// </summary>
        private SchemeObject vals;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateLetRec class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="body">The let body.</param>
        /// <param name="vars">The variables to be bound.</param>
        /// <param name="inits">The initialization expressions.</param>
        private EvaluateLetRec(SchemeObject expr, Environment env, Evaluator caller, SchemeObject body, SchemeObject vars, SchemeObject inits)
            : base(EvalInitStep, expr, env, caller)
        {
            this.body = body;
            this.vars = vars;
            this.inits = inits;
            this.formals = vars;
            this.vals = EmptyList.Instance;
        }
        #endregion

        #region Call
        /// <summary>
        /// Call letrec evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The let evaluator.</returns>
        internal static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            if (expr is EmptyList)
            {
                ErrorHandlers.SemanticError("No arguments for letrec", null);
            }

            if (!(expr is Pair))
            {
                ErrorHandlers.SemanticError("Bad arg list for letrec: " + expr, null);
            }

            SchemeObject body = Rest(expr);
            if (body is EmptyList)
            {
                caller.ReturnedExpr = Undefined.Instance;
                return caller;
            }

            SchemeObject bindings = First(expr);
            //// In the bindings, the variables are first, the values are second, and anything left over is discarded.
            //// If either is missing, an empty list is used instead.
            //// To start with, bindings are undefined.
            SchemeObject formals = MapFun(First, bindings);
            SchemeObject inits = MapFun(Second, bindings);
            SchemeObject initVals = Fill(ListLength(formals), Undefined.Instance);

            return new EvaluateLetRec(expr, new Environment(formals, initVals, env), caller, body, formals, inits);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Evaluate one of the inits in the environment of the vars.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next step.</returns>
        private static Evaluator EvalInitStep(Evaluator s)
        {
            var step = (EvaluateLetRec)s;
            if (step.inits is EmptyList)
            {
                s.Pc = ApplyProcStep;
                return s;
            }

            Lambda fun = Lambda.New(step.formals, MakeList(First(step.inits)), s.Env);
            s.Pc = BindVarToInitStep;
            return fun.ApplyWithtEnv(s.Env, s);
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
            var step = (EvaluateLetRec)s;
            step.vals = Cons(s.ReturnedExpr, step.vals);
            step.vars = Rest(step.vars);
            step.inits = Rest(step.inits);
            s.Pc = EvalInitStep;
            return s;
        }

        /// <summary>
        /// Inits evaluated and bound -- execute the proc and return.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Execution returns to the caller.</returns>
        private static Evaluator ApplyProcStep(Evaluator s)
        {
            var step = (EvaluateLetRec)s;

            // assign the inits into the env
            SchemeObject vars = step.formals;
            SchemeObject vals = ReverseList(step.vals);
            step.UpdateEnvironment(vars, vals);

            // apply the fun to the vals and return
            Lambda fun = Lambda.New(step.formals, step.body, s.Env);
            return fun.ApplyWithtEnv(s.Env, s.Caller);
        }
        #endregion
    }
}