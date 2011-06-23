// <copyright file="Eval.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// Template for an evaluation procedure.
    /// </summary>
    /// <returns>The stepper to execute next.</returns>
    public delegate Stepper Stepper();

    /// <summary>
    /// Evaluates expressions step by step.
    /// </summary>
    public class Evaluator : SchemeUtils
    {
        /// <summary>
        /// The scheme interpreter.
        /// </summary>
        private readonly Scheme interp;

        /// <summary>
        /// The parent evaluator.
        /// Return to this one when done.
        /// </summary>
        private readonly Stepper parent;

        /// <summary>
        /// The expression being evaluated.  During evaluation, this is 
        ///   replaced by the results of evaluation.  At the conclusion of evaluation
        ///   this holds the final result.
        /// </summary>
        private object expr;

        /// <summary>
        /// The evaluation environment.  If the environment is changes as a result
        ///   of evaluation, then the new environment is here after evaluation.
        /// </summary>
        private Environment env;

        /// <summary>
        /// A program oounter.
        /// </summary>
        private int pc;

        /// <summary>
        /// Initializes a new instance of the Evaluator class.
        /// </summary>
        /// <param name="interp">The scheme interpreter.</param>
        /// <param name="parent">The parent evaluator.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluator environment.</param>
        public Evaluator(Scheme interp, Stepper parent, object expr, Environment env)
        {
            this.interp = interp;
            this.parent = parent;
            this.expr = expr;
            this.env = env;
            this.pc = 0;
        }

        /// <summary>
        /// Gets the expression being evaluated.  Aftet execution is done, this is the
        ///     evaluation result.
        /// </summary>
        public object Expr { get { return expr; } }

        /// <summary>
        /// Gets the evaluation environment.  After execution, this is the new environment.
        /// </summary>
        public Environment Env { get { return env; } }

        /// <summary>
        /// This is not meant to be called, but is returned as an indication that
        ///   evaluation is complete.  The resulting value and new environment are
        ///   in Expr and Env.
        /// </summary>
        /// <returns>Does not return.</returns>
        public static Stepper EvalReturn()
        {
            throw new Exception("This should not be called");
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
        public Stepper EvalStep()
        {
            if (expr is string)
            {
                // Evaluate a string by looking it up in the environment.
                // It should correspond to a varialbe name, for which there 
                //    is a corresponding value.
                expr = env.Lookup((string)expr);
                return this.parent;
            }

            if (!(expr is Pair))
            {
                // If we are evaluating something that is not a pair, 
                //    it must be a constant.
                // Return the integer, real, boolean, or vector.
                return this.parent;
            }

            // We are evaluating a pair.
            // Split out the first item for special treatment.
            object fn = First(expr);
            object args = Rest(expr);

            // Look for one of the special forms. 
            switch (fn as string)
            {
                case "quote":
                    // Evaluate quoted expression by just returning the expression.
                    expr = First(args);
                    return this.parent;

                case "begin":
                    {
                        // Evaluate begin by evaluating all the items in order, 
                        //   and returning the last.
                        var eval = new Evaluator(this.interp, this.EvalStep, args, env);
                        var next = eval.EvalSequence();
                        expr = eval.Expr;
                        return next;
                    }

                case "define":
                    {
                        // Define is a shortcut for lambda.
                        // Evaluate by splicing lambda on the front and evaluating that.
                        this.EvalDefine(args);
                        return this.parent;
                    }

                case "set!":
                    {
                        // Evaluate a set! expression by evaluating the second, 
                        //   then setting the first to it.
                        var eval = new Evaluator(this.interp, this.EvalStep, args, env);
                        var next = eval.EvalSet();
                        expr = eval.Expr;
                        return next;
                    }

                case "if":
                    {
                        // Eval an if expression by evaluating the first clause, 
                        //    and then returning either the second or third.
                        var eval = new Evaluator(this.interp, this.EvalStep, args, env);
                        var next = eval.EvalIf();
                        expr = eval.Expr;
                        return next;
                    }

                case "cond":
                    expr = this.ReduceCond(args);
                    return this.EvalStep;

                case "lambda":
                    // Evaluate a lambda by creating a closure.
                    expr = new Closure(First(args), Rest(args), env);
                    return this.parent;

                case "macro":
                    // Evaluate a macro by creating a macro.
                    expr = new Macro(First(args), Rest(args), env);
                    return this.parent;
            }

            // If we get here, it wasn't one of the special forms.  
            // So we need to evaluate the first item (the function) in preparation for
            //    doing a procedure call.
            fn = this.interp.Eval(fn, env);

            // If the function is a macro, expand it and then continue.
            if (fn is Macro)
            {
                Macro m = (Macro)fn;
                expr = m.Expand(this.interp, (Pair)expr, args);
                return this.EvalStep;
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
                expr = f.Body;
                env = new Environment(f.Parms, this.EvalList(args), f.Env);
                return this.EvalStep;
            }

            // This is a procedure call.
            // In any other case, the function is a primitive or a user-defined function.
            // Evaluate the arguments in the environment, then apply the function 
            //    to the arguments.
            expr = this.ApplyProc(fn, args);
            return this.parent;
        }

        /// <summary>
        /// Evaluate a define expression.
        /// Handle the define syntax shortcut by inserting lambda.
        /// </summary>
        /// <param name="args">The body of the define</param>
        /// <returns>The evaluated definition.</returns>
        private void EvalDefine(object args)
        {
            object value;
            if (First(args) is Pair)
            {
                value = this.interp.Eval(Cons("lambda", Cons(Rest(First(args)), Rest(args))), env);
                expr = env.Define(First(First(args)), value);
            }
            else
            {
                value = this.interp.Eval(Second(args), env);
                expr = env.Define(First(args), value);
            }
        }

        /// <summary>
        /// Evaluate a set! expression.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        private Stepper EvalSet()
        {
            var args = expr;
            object temp = this.interp.Eval(Second(args), env);
            expr = env.Set(First(args), temp);
            return this.parent;
        }

        /// <summary>
        /// Evaluate an if expression.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        private Stepper EvalIf()
        {
            var args = expr;
            object temp = this.interp.Eval(First(args), env);
            expr = Truth(temp) ? Second(args) : Third(args);
            return this.parent;
        }

        /// <summary>
        /// Evaluate a sequence of objects, returning the last.
        /// </summary>
        /// <returns>The next step.</returns>
        private Stepper EvalSequence()
        {
            var args = expr;
            while (Rest(args) != null)
            {
                this.interp.Eval(First(args), env);
                args = Rest(args);
            }

            expr = First(args);
            return this.parent;
        }

        /// <summary>
        /// Evaluate the items in a list, given the environment.
        /// This is done to the args of a procedure call (except for special forms).
        /// This is an iterative, rather than a recursive one.
        /// </summary>
        /// <param name="list">The list of items to evaluate.</param>
        /// <returns>A list of the evaluated items.</returns>
        private Pair EvalList(object list)
        {
            if (list == null)
            {
                return null;
            }

            if (!(list is Pair))
            {
                Error("Illegal arg list: " + list);
                return null;
            }

            Pair result = List(null); // empty cell will be stripped off below
            Pair accum = result;
            while (list is Pair)
            {
                object value = this.interp.Eval(First(list), env);
                accum = (Pair)(accum.Rest = List(value));
                list = Rest(list);
            }

            return (Pair)result.Rest;
        }

        /// <summary>
        /// Handle a cond by iterating down the list of clauses.
        /// The clauses are (guard expression) pairs.
        /// If they are exhausted, return False.
        /// If we find a True guard or an else, then:
        ///     If the clause has no expression, return the guard.
        ///     Otherwise return the expression to be evaluated.
        /// </summary>
        /// <param name="clauses">The clauses to evaluate</param>
        /// <returns>Another expression to evaluate instead.</returns>
        private object ReduceCond(object clauses)
        {
            object result = null;
            while (true)
            {
                if (clauses == null)
                {
                    return False;
                }

                object clause = First(clauses);
                clauses = Rest(clauses);
                bool convert;
                if (First(clause) as string == "else")
                {
                    convert = true;
                }
                else
                {
                    result = this.interp.Eval(First(clause), env);
                    convert = Truth(result);
                }

                if (convert)
                {
                    if (Rest(clause) == null)
                    {
                        return List("quote", result);
                    }

                    if (Second(clause) as string == "=>")
                    {
                        return List(Third(clause), List("quote", result));
                    }

                    return Cons("begin", Rest(clause));
                }
            }
        }

        /// <summary>
        /// Apply the function to the args in a given environment.
        /// </summary>
        /// <param name="fn">The proc to apply.</param>
        /// <param name="args">The proc args.</param>
        /// <returns>The result of the application.</returns>
        private object ApplyProc(object fn, object args)
        {
            return Procedure.Proc(fn).Apply(this.interp, this.EvalList(args));
        }
    }
}
