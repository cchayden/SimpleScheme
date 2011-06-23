// <copyright file="EvaluateLet.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a let expression
    /// Bind the variables to values and then evaluate the expressions.
    /// </summary>
    public sealed class EvaluateLet : Stepper
    {
        /// <summary>
        /// The variable bindings established by the let expression.
        /// </summary>
        private object bindings;

        /// <summary>
        /// Initializes a new instance of the EvaluateLet class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateLet(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
        }

        /// <summary>
        /// Call a let evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The let evaluator.</returns>
        public static EvaluateLet Call(Stepper caller, object expr)
        {
            return new EvaluateLet(caller, expr, caller.Env);
        }

        /// <summary>
        /// Evaluate the values, collecting them into a list.
        /// Collect the variables into another list.
        /// Create a lambda and evaluate that.
        /// </summary>
        /// <returns>The next step.</returns>
        public override Stepper RunStep()
        {
            while (true)
            {
                switch (this.Pc)
                {
                    case PC.Initial:
                        object body;
                        object name = null;
                        if (this.Expr == null)
                        {
                            ErrorHandlers.Error("Let: wrong number of arguments");
                            return ReturnFromStep(null);
                        }

                        if (List.First(this.Expr) is string)
                        {
                            // named let
                            name = List.First(this.Expr);
                            this.bindings = List.First(List.Rest(this.Expr));
                            body = List.Rest(List.Rest(this.Expr));
                        }
                        else
                        {
                            this.bindings = List.First(this.Expr);
                            body = List.Rest(this.Expr);
                        }

                        if (body == null)
                        {
                            return ReturnFromStep(null);
                        }

                        if (!(this.Expr is Pair))
                        {
                            ErrorHandlers.Error("Let: illegal arg list: " + this.Expr);
                            return ReturnFromStep(null);
                        }

                        object vars = List.MapFun(List.First, List.MakeList(this.bindings));
                        object innerLambda = List.Cons("lambda", List.Cons(vars, body));
                        if (name == null)
                        {
                            this.Pc = PC.Step1;
                            return EvaluatorMain.Call(this, innerLambda);
                        } 

                        // named let -- create an outer lambda that defines the name
                        object set = List.MakeList("set!", name, innerLambda);
                        object vals = List.MapFun(List.Second, List.MakeList(this.bindings));
                        object invoke = List.Cons(name, vals);
                        object letExpr = List.MakeList("lambda", List.MakeList(name), set, invoke);
                        this.Pc = PC.Step2;
                        return EvaluatorMain.Call(this, letExpr);

                    case PC.Step1:
                        // the rest of regular let
                        Pc = PC.Step3;
                        return EvaluateApplyProc.Call(this, List.MapFun(List.Second, List.MakeList(this.bindings)), ReturnedExpr);

                    case PC.Step2:
                        // the rest of named let -- apply the lambda we defined before
                        Pc = PC.Step3;
                        return EvaluateApplyProc.Call(this, List.MakeList(SchemeBoolean.False), ReturnedExpr);

                    case PC.Step3:
                        return ReturnFromStep(this.ReturnedExpr);
                }

                return ErrorHandlers.EvalError("Let: program counter error");
            }
        }
    }
}