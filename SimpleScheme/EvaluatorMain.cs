// <copyright file="EvaluatorMain.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// The main evaluator for expressions.
    /// </summary>
    public sealed class EvaluatorMain : Stepper
    {
        /// <summary>
        /// A function to be evaluated (primitive, proc, macro, or closure).
        /// </summary>
        private object fn;

        /// <summary>
        /// The arguments to the function.
        /// </summary>
        private object args;

        /// <summary>
        /// Initializes a new instance of the EvaluatorMain class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluatorMain(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
        }

        /// <summary>
        /// Calls the main evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The evaluator.</returns>
        public static EvaluatorMain Call(Stepper caller, object expr)
        {
            return new EvaluatorMain(caller, expr, caller.Env);
        }

        /// <summary>
        /// Calls the main evaluator.
        /// This version uses a supplied environment.  This is used with a closure
        ///   to evaluate in the closure's environment rather than in the current one.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to evaluate in.</param>
        /// <returns>The evaluator.</returns>
        public static EvaluatorMain Call(Stepper caller, object expr, Environment env)
        {
            return new EvaluatorMain(caller, expr, env);
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
        public override Stepper RunStep()
        {
            while (true)
            {
                switch (this.Pc)
                {
                    case PC.Initial:
                        if (this.Expr is string)
                        {
                            // Evaluate a string by looking it up in the environment.
                            // It should correspond to a varialbe name, for which there 
                            //    is a corresponding value.
                            return ReturnFromStep(EvalString((string)this.Expr, this.Env));
                        }

                        if (!(this.Expr is Pair))
                        {
                            // If we are evaluating something that is not a pair, 
                            //    it must be a constant.
                            // Return the integer, real, boolean, or vector.
                            return ReturnFromStep(this.Expr);
                        }

                        // We are evaluating a pair.
                        // Split out the first item for special treatment.
                        this.fn = List.First(this.Expr);
                        this.args = List.Rest(this.Expr);
//                        Console.WriteLine("{0} {1}", this.fn, this.args);

                        // Look for one of the special forms. 
                        switch (this.fn as string)
                        {
                            case "quote":
                                // Evaluate quoted expression by just returning the expression.
                                return ReturnFromStep(List.First(this.args));

                            case "begin":
                                {
                                    // Evaluate begin by evaluating all the items in order, 
                                    //   and returning the last.
                                    return GoToStep(EvaluateSequence.Call(this, this.args), PC.Step2);
                                }

                            case "define":
                                {
                                    // Define is a shortcut for lambda.
                                    // Evaluate by splicing lambda on the front and evaluating that.
                                    return GoToStep(EvaluateDefine.Call(this, this.args), PC.Step3);
                                }

                            case "set!":
                                {
                                    // Evaluate a set! expression by evaluating the second, 
                                    //   then setting the first to it.
                                    return GoToStep(EvaluateSet.Call(this, this.args), PC.Step3);
                                }

                            case "if":
                                {
                                    // Eval an if expression by evaluating the first clause, 
                                    //    and then returning either the second or third.
                                    return GoToStep(EvaluateIf.Call(this, this.args), PC.Step2);
                                }

                            case "or":
                                {
                                    return GoToStep(EvaluateOr.Call(this, this.args), PC.Step3);
                                }

                            case "and":
                                {
                                    return GoToStep(EvaluateAnd.Call(this, this.args), PC.Step3);
                                }

                            case "cond":
                                return GoToStep(EvaluateCond.Call(this, this.args), PC.Step2);

                            case "let":
                                return GoToStep(EvaluateLet.Call(this, this.args), PC.Step3);

                            case "lambda":
                                // Evaluate a lambda by creating a closure.
                                return ReturnFromStep(new Closure(List.First(this.args), List.Rest(this.args), this.Env));

                            case "macro":
                                // Evaluate a macro by creating a macro.
                                return ReturnFromStep(new Macro(List.First(this.args), List.Rest(this.args), this.Env));
                        }

                        // If we get here, it wasn't one of the special forms.  
                        // So we need to evaluate the first item (the function) in preparation for
                        //    doing a procedure call.
                        return GoToStep(EvaluatorMain.Call(this, this.fn), PC.Step1);

                    case PC.Step1:
                        // Come here after evaluating fn
                        // Make sure we don't assigne expr, since we need it intact below.
                        this.fn = ReturnedExpr;

                        // If the function is a macro, expand it and then continue.
                        if (this.fn is Macro)
                        {
                            return GoToStep(EvaluateExpandMacro.Call(this, this.args, (Macro)this.fn), PC.Step2);
                        }

                        // If the function is a closure, then create a new environment consisting of
                        //   1 the closure param list
                        //   2 arguments evaluated in the original environment
                        //   3 the closure's environment
                        // Then continue evaluating the closure body in this new environment
                        if (this.fn is Closure)
                        {
                            // CLOSURE CALL -- capture the environment and continue with the body
                            return GoToStep(EvaluateClosure.Call(this, this.args, (Closure)this.fn), PC.Step2);
                        }

                        // This is a procedure call.
                        // In any other case, the function is a primitive or a user-defined function.
                        // Evaluate the arguments in the environment, then apply the function 
                        //    to the arguments.
                        return GoToStep(EvaluateApplyProc.Call(this, this.args, this.fn), PC.Step3);

                    case PC.Step2:
                        // Loop: copy results of return to Expr/Env and evaluate again
                        this.Expr = this.ReturnedExpr;
                        this.Env = this.ReturnedEnv;
                        Pc = PC.Initial;
                        continue;

                    case PC.Step3:
                        // Assign return value and return to caller.
                        return ReturnFromStep(this.ReturnedExpr, this.ReturnedEnv);
                }

                return ErrorHandlers.EvalError("EvaluatorMain: program counter error");
            }
        }

        /// <summary>
        /// Evaluate a string by looking it up in the environment.
        /// </summary>
        /// <param name="symbol">The symbol</param>
        /// <param name="env">The environment to evaluate it in.</param>
        /// <returns>The value of the symbol.</returns>
        private static object EvalString(string symbol, Environment env)
        {
            return env.Lookup(symbol);
        }
    }
}