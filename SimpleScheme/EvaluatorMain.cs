#define NEW
// <copyright file="EvaluatorMain.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    public partial class Evaluator
    {
        /// <summary>
        /// The main evaluator for expressions.
        /// </summary>
        private class EvaluatorMain : Evaluator
        {
            private object fn;
            private object args;

            /// <summary>
            /// Initializes a new instance of the EvaluatorMain class.
            /// </summary>
            /// <param name="interp">The interpreter.</param>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            public EvaluatorMain(Scheme interp, Evaluator parent, object expr, Environment env)
                : base(interp, parent, expr, env)
            {
            }

            /// <summary>
            /// Evaluate an expression in the given environment, one step at a time.
            /// The expression and environment are given in the constructor.
            /// Besides all the special forms, the main action is to:
            ///   return strings and constants, which evaluate to themselves
            ///   treat the first arg as a procedure, evaluate the rest of the expression args, 
            ///   and apply the procedure to the evaluated results.
            /// </summary>
            /// <returns>The next step to execute.</returns>
            public override Evaluator EvalStep()
            {
                switch (this.Pc)
                {
                    case PcReturn:
                        // copy results of call, then return to caller
                        this.RetExpr = this.ReturnedExpr;
                        this.RetEnv = this.ReturnedEnv;
                        return EvalReturn();

                    case 0:
                        if (this.Expr is string)
                        {
                            // Evaluate a string by looking it up in the environment.
                            // It should correspond to a varialbe name, for which there 
                            //    is a corresponding value.
                            this.RetExpr = this.Env.Lookup((string)this.Expr);
                            return EvalReturn();
                        }

                        if (!(this.Expr is Pair))
                        {
                            // If we are evaluating something that is not a pair, 
                            //    it must be a constant.
                            // Return the integer, real, boolean, or vector.
                            this.RetExpr = this.Expr;
                            return EvalReturn();
                        }

                        // We are evaluating a pair.
                        // Split out the first item for special treatment.
                        fn = First(this.Expr);
                        args = Rest(this.Expr);

                        // Look for one of the special forms. 
                        switch (fn as string)
                        {
                            case "quote":
                                // Evaluate quoted expression by just returning the expression.
                                this.RetExpr = First(args);
                                return EvalReturn();

                            case "begin":
                                {
                                    // Evaluate begin by evaluating all the items in order, 
                                    //   and returning the last.
                                    Pc = 1;
                                    return CallSequence(args);
                                }

                            case "define":
                                {
                                    // Define is a shortcut for lambda.
                                    // Evaluate by splicing lambda on the front and evaluating that.
                                    return CallDefine(args);
                                }

                            case "set!":
                                {
                                    // Evaluate a set! expression by evaluating the second, 
                                    //   then setting the first to it.
                                    return CallSet(args);
                                }

                            case "if":
                                {
                                    // Eval an if expression by evaluating the first clause, 
                                    //    and then returning either the second or third.
                                    Pc = 1;
                                    return CallIf(args);
                                }

                            case "cond":
                                Pc = 1;
                                return CallReduceCond(args);

                            case "lambda":
                                // Evaluate a lambda by creating a closure.
                                this.RetExpr = new Closure(First(args), Rest(args), this.Env);
                                return EvalReturn();

                            case "macro":
                                // Evaluate a macro by creating a macro.
                                this.RetExpr = new Macro(First(args), Rest(args), this.Env);
                                return EvalReturn();
                        }

                        // If we get here, it wasn't one of the special forms.  
                        // So we need to evaluate the first item (the function) in preparation for
                        //    doing a procedure call.
                        Pc = 2;
                        return CallEval(fn);

                    case 1:
                        // loop: copy results of return to Expr/Env and evaluate again
                        this.Expr = this.ReturnedExpr;
                        this.Env = this.ReturnedEnv;
                        Pc = 0;
                        return this;

                    case 2:
                        // Come here after evaluating fn
                        // make sure we don't assigne expr, since we need it intact below
                        fn = ReturnedExpr;

                        // If the function is a macro, expand it and then continue.
                        if (fn is Macro)
                        {
                            Macro m = (Macro)fn;
                            this.Expr = m.Expand(this.Interp, (Pair)this.Expr, args);
                            Pc = 0;
                            return EvalContinue();
                        }

                        // If the function is a closure, then create a new environment consisting of
                        //   1 the closure param list
                        //   2 arguments evaluated in the original environment
                        //   3 the closure's environment
                        // Then continue evaluating the closure body in this new environment
                        if (fn is Closure)
                        {
                            // CLOSURE CALL -- capture the environment and continue with the body
                            Closure f = (Closure)fn;
                            Pc = 1;
                            return CallClosure(args, f);
                        }

                        // This is a procedure call.
                        // In any other case, the function is a primitive or a user-defined function.
                        // Evaluate the arguments in the environment, then apply the function 
                        //    to the arguments.
                        return CallApplyProc(args, fn);
                }
                throw new Exception("EvaluatorMain pc error");
            }
        }
    }
}