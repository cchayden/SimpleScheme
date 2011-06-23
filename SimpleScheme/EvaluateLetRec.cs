// <copyright file="EvaluateLetRec.cs" company="Charles Hayden">
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
    public sealed class EvaluateLetRec : Stepper
    {
        /// <summary>
        /// The body of the let.
        /// </summary>
        private object body;

        /// <summary>
        /// The list of variables to bind.
        /// </summary>
        private object vars;

        /// <summary>
        /// This list of initial expressions.
        /// </summary>
        private object inits;

        /// <summary>
        /// The list of formal parameters to pass to the final closure.
        /// This is variable1, variable2, ...
        /// </summary>
        private object formals;

        /// <summary>
        /// Accumulate the list of init vals here for later assignment.
        /// </summary>
        private System.Collections.Generic.List<object> vals;

        /// <summary>
        /// Initializes a new instance of the EvaluateLetRec class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateLetRec(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
            this.Pc = this.InitialStep;
            IncrementCounter("letrec");
        }

        /// <summary>
        /// Call letrec evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The let evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
        {
            return new EvaluateLetRec(caller, expr, caller.Env);
        }

        /// <summary>
        /// Start by checking the number of arguments.
        /// Then test for named let.  
        /// Grab bindings, body, vars, and inits.
        /// Make an environment for evaluating the inits consisting of the vars and 
        ///   an equal number of #f.
        /// </summary>
        /// <returns>Continues by evaluating the init list.</returns>
        private Stepper InitialStep()
        {
            if (this.Expr == null)
            {
                ErrorHandlers.Error("Letrec: wrong number of arguments");
                return ReturnFromStep(null);
            }

            if (!(this.Expr is Pair))
            {
                ErrorHandlers.Error("Letrec: illegal arg list: " + this.Expr);
                return ReturnFromStep(null);
            }

            object bindings = List.First(this.Expr);
            this.body = List.Rest(this.Expr);

            if (this.body == null)
            {
                return ReturnFromStep(null);
            }

            this.vars = List.MapFun(List.First, List.MakeList(bindings));
            this.inits = List.MapFun(List.Second, List.MakeList(bindings));
            this.formals = this.vars;
            object initVals = null;
            int n = List.Length(this.vars);
            for (int i = 0; i < n; i++)
            {
                initVals = List.Cons(false, initVals);
            }

            this.vals = new System.Collections.Generic.List<object>(n);

            this.Env = new Environment(this.formals, initVals, this.Parent.Env);
            this.Pc = this.EvalInit;
            return this;
        }

        /// <summary>
        /// Evaluate one of the inits in the environment of the vars.
        /// </summary>
        /// <returns>The next step.</returns>
        private Stepper EvalInit()
        {
            if (this.inits == null)
            {
                this.Pc = this.ApplyProc;
                return this;
            }

            Closure fun = new Closure(this.formals, List.MakeList(List.First(this.inits)), this.Env);  
            this.Pc = this.BindVarToInit;
            return fun.ApplyWithEnv(this);
        }

        /// <summary>
        /// Add var to list of formals, evaluation result to list of vals.
        /// Step down list of vars and inits.
        /// Go back and evaluate another init.
        /// </summary>
        /// <returns>The next step.</returns>
        private Stepper BindVarToInit()
        {
            this.vals.Add(ReturnedExpr);
            this.vars = List.Rest(this.vars);
            this.inits = List.Rest(this.inits);
            this.Pc = this.EvalInit;
            return this;
        }

        /// <summary>
        /// Inits evaluated and bound -- execute the proc and return.
        /// </summary>
        /// <returns>Execution returns to the caller.</returns>
        private Stepper ApplyProc()
        {
            // assign the inits into the env
            object var = this.formals;
            int n = List.Length(this.formals);
            for (int i = 0; i < n; i++)
            {
                this.Env.Set(List.First(var), this.vals[i]);
                var = List.Rest(var);
            }

            // apply the fun to the vals and return
            Closure fun = new Closure(this.formals, this.body, this.Env);
            this.Pc = this.ReturnStep;
            return fun.ApplyWithEnv(this);
        }
    }
}