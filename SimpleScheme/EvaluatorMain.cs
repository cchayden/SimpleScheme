// <copyright file="EvaluatorMain.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Stepper contains all the individual evaluators
    /// </summary>
    public partial class Stepper
    {
        /// <summary>
        /// Evaluate a string by looking it up in the environment.
        /// </summary>
        /// <param name="symbol">The symbol</param>
        /// <param name="env">The environment to evaluate it in.</param>
        /// <returns>The value of the symbol.</returns>
        public static object EvalString(string symbol, Environment env)
        {
            return env.Lookup(symbol);
        }

        /// <summary>
        /// Evaluate a string in the global environment, by looking it up.
        /// </summary>
        /// <param name="symbol">The symbol</param>
        /// <param name="interp">The current interpreter.</param>
        /// <returns>The value of the symbol.</returns>
        public static object GlobalEvalString(string symbol, Scheme interp)
        {
            return interp.GlobalEnvironment.Lookup(symbol);
        }

        /// <summary>
        /// The main evaluator for expressions.
        /// </summary>
        private class EvaluatorMain : Stepper
        {
            /// <summary>
            /// Starting Pc value
            /// </summary>
            private const int PcBegin = 0;

            /// <summary>
            /// Pc value when evaluating a primitive, proc, macro or closure.
            /// </summary>
            private const int PcMiddle = 1;

            /// <summary>
            /// Pc value to loop beck and evaluate the result of the
            ///   previous evaluation step.
            /// </summary>
            private const int PcLoop = 2;

            /// <summary>
            /// Pc value to set the return value and return to the caller.
            /// </summary>
            private const int PcReturn = 3;

            /// <summary>
            /// A function to be evaluated (primitive, proc, macro, or closure).
            /// </summary>
            private object fn;

            /// <summary>
            /// The arguments to the function.
            /// </summary>
            private object args;
            
            /// <summary>
            /// Initializes a new instance of the Stepper.EvaluatorMain class.
            /// </summary>
            /// <param name="interp">The interpreter.</param>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            public EvaluatorMain(Scheme interp, Stepper parent, object expr, Environment env)
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
            public override Stepper EvalStep()
            {
                switch (this.Pc)
                {
                    case PcBegin:
                        if (this.Expr is string)
                        {
                            // Evaluate a string by looking it up in the environment.
                            // It should correspond to a varialbe name, for which there 
                            //    is a corresponding value.
                            return SubReturn(EvalString((string)this.Expr, this.Env));
                        }

                        if (!(this.Expr is Pair))
                        {
                            // If we are evaluating something that is not a pair, 
                            //    it must be a constant.
                            // Return the integer, real, boolean, or vector.
                            return SubReturn(this.Expr);
                        }

                        // We are evaluating a pair.
                        // Split out the first item for special treatment.
                        this.fn = First(this.Expr);
                        this.args = Rest(this.Expr);

                        // Look for one of the special forms. 
                        switch (this.fn as string)
                        {
                            case "quote":
                                // Evaluate quoted expression by just returning the expression.
                                return SubReturn(First(this.args));

                            case "begin":
                                {
                                    // Evaluate begin by evaluating all the items in order, 
                                    //   and returning the last.
                                    Pc = PcLoop;
                                    return CallSequence(this.args);
                                }

                            case "define":
                                {
                                    // Define is a shortcut for lambda.
                                    // Evaluate by splicing lambda on the front and evaluating that.
                                    Pc = PcReturn;
                                    return CallDefine(this.args);
                                }

                            case "set!":
                                {
                                    // Evaluate a set! expression by evaluating the second, 
                                    //   then setting the first to it.
                                    Pc = PcReturn;
                                    return CallSet(this.args);
                                }

                            case "if":
                                {
                                    // Eval an if expression by evaluating the first clause, 
                                    //    and then returning either the second or third.
                                    Pc = PcLoop;
                                    return CallIf(this.args);
                                }

                            case "cond":
                                Pc = PcLoop;
                                return CallReduceCond(this.args);

                            case "lambda":
                                // Evaluate a lambda by creating a closure.
                                return SubReturn(new Closure(First(this.args), Rest(this.args), this.Env));

                            case "macro":
                                // Evaluate a macro by creating a macro.
                                return SubReturn(new Macro(First(this.args), Rest(this.args), this.Env));
                        }

                        // If we get here, it wasn't one of the special forms.  
                        // So we need to evaluate the first item (the function) in preparation for
                        //    doing a procedure call.
                        Pc = PcMiddle;
                        return CallEval(this.fn);

                    case PcMiddle:
                        // Come here after evaluating fn
                        // make sure we don't assigne expr, since we need it intact below
                        this.fn = ReturnedExpr;

                        // If the function is a macro, expand it and then continue.
                        if (this.fn is Macro)
                        {
                            Pc = PcLoop;
                            return CallExpand(this.args, (Macro)this.fn);
                        }

                        // If the function is a closure, then create a new environment consisting of
                        //   1 the closure param list
                        //   2 arguments evaluated in the original environment
                        //   3 the closure's environment
                        // Then continue evaluating the closure body in this new environment
                        if (this.fn is Closure)
                        {
                            // CLOSURE CALL -- capture the environment and continue with the body
                            Pc = PcLoop;
                            return CallClosure(this.args, (Closure)this.fn);
                        }

                        // This is a procedure call.
                        // In any other case, the function is a primitive or a user-defined function.
                        // Evaluate the arguments in the environment, then apply the function 
                        //    to the arguments.
                        Pc = PcReturn;
                        return CallApplyProc(this.args, this.fn);

                    case PcLoop:
                        // Loop: copy results of return to Expr/Env and evaluate again
                        this.Expr = this.ReturnedExpr;
                        this.Env = this.ReturnedEnv;
                        Pc = PcBegin;
                        return this;

                    case PcReturn:
                        // Assign return value and return to caller.
                        this.Env = this.ReturnedEnv;
                        return SubReturn(this.ReturnedExpr);
                }

                return EvalError("EvaluatorMain: program counter error");
            }
        }
    }
}