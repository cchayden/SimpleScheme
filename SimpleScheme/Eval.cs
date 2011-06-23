// <copyright file="Eval.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// Template for an evaluation procedure.
    /// </summary>
    /// <param name="expr"> The expression to evaluate. </param>
    /// <param name="env"> The environment for the evaluation. </param>
    /// <param name="res">
    /// The intermediate or final result. </param>
    /// <param name="resEnv">The next environment  </param>
    /// <returns>The stepper to execute next.</returns>
    public delegate Stepper Stepper(object expr, Environment env, out object res, out Environment resEnv);

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

        private object expr;

        /// <summary>
        /// Initializes a new instance of the Evaluator class.
        /// </summary>
        /// <param name="interp">The scheme interpreter.</param>
        /// <param name="parent">The parent evaluator.</param>
        public Evaluator(Scheme interp, Stepper parent)
        {
            this.interp = interp;
            this.parent = parent;
        }

        /// <summary>
        /// This is not meant to be called, but is returned as a return signal.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to evaluate it in.</param>
        /// <param name="res">The result of the evaluation step.</param>
        /// <param name="resEnv">The new environment.</param>
        /// <returns>The next step to execute..</returns>
        public static Stepper EvalReturn(object expr, Environment env, out object res, out Environment resEnv)
        {
            throw new Exception("This should not be called");
        }

        /// <summary>
        /// Evaluate an expression in the given environment, one step at a time.
        /// Besides all the special forms, the main action is to:
        ///   return strings and constants, which evaluate to themselves
        ///   treat the first arg as a procedure, evaluate the rest of the expression args, 
        ///   and apply the procedure to the evaluated results.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to evaluate it in.</param>
        /// <param name="res">The result of the evaluation step.</param>
        /// <param name="resEnv">The new environment.</param>
        /// <returns>The next step to execute..</returns>
        public Stepper EvalStep(object inExpr, Environment env, out object res, out Environment resEnv)
        {
            this.expr = inExpr;
            resEnv = env;
            if (expr is string)
            {
                // Evaluate a string by looking it up in the environment.
                // It should correspond to a varialbe name, for which there 
                //    is a corresponding value.
                res = env.Lookup((string)expr);
                return this.parent;
            }

            if (!(expr is Pair))
            {
                // If we are evaluating something that is not a pair, 
                //    it must be a constant.
                // Return the integer, real, boolean, or vector.
                res = expr;
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
                    res = First(args);
                    return this.parent;

                case "begin":
                    // Evaluate begin by evaluating all the items in order, 
                    //   and returning the last.
                    return new Evaluator(this.interp, this.EvalStep).EvalSequence(args, env, out res, out resEnv);

                case "define":
                    // Define is a shortcut for lambda.
                    // Evaluate by splicing lambda on the front and evaluating that.
                    res = this.EvalDefine(args, env);
                    return this.parent;

                case "set!":
                    // Evaluate a set! expression by evaluating the second, 
                    //   then setting the first to it.
                    return new Evaluator(this.interp, this.EvalStep).EvalSet(args, env, out res, out resEnv);

                case "if":
                    // Eval an if expression by evaluating the first clause, 
                    //    and then returning either the second or third.
                    return new Evaluator(this.interp, this.EvalStep).EvalIf(args, env, out res, out resEnv);

                case "cond":
                    res = this.ReduceCond(args, env);
                    return this.EvalStep;

                case "lambda":
                    // Evaluate a lambda by creating a closure.
                    res = new Closure(First(args), Rest(args), env);
                    return this.parent;

                case "macro":
                    // Evaluate a macro by creating a macro.
                    res = new Macro(First(args), Rest(args), env);
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
                res = m.Expand(this.interp, (Pair)expr, args);
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
                res = f.Body;
                resEnv = new Environment(f.Parms, this.EvalList(args, env), f.Env);
                return this.EvalStep;
            }

            // This is a procedure call.
            // In any other case, the function is a primitive or a user-defined function.
            // Evaluate the arguments in the environment, then apply the function 
            //    to the arguments.
            res = this.ApplyProc(fn, args, env);
            return this.parent;
        }

        /// <summary>
        /// Evaluate a define expression.
        /// Handle the define syntax shortcut by inserting lambda.
        /// </summary>
        /// <param name="args">The body of the define</param>
        /// <param name="env">The evaluation environment.</param>
        /// <returns>The evaluated definition.</returns>
        private object EvalDefine(object args, Environment env)
        {
            if (First(args) is Pair)
            {
                return env.Define(
                    First(First(args)),
                    this.interp.Eval(Cons("lambda", Cons(Rest(First(args)), Rest(args))), env));
            }

            return env.Define(First(args), this.interp.Eval(Second(args), env));
        }

        /// <summary>
        /// Evaluate a set! expression.
        /// </summary>
        /// <param name="args">The arguments to set!</param>
        /// <param name="env">The evaluation environment.</param>
        /// <param name="res">The result of the evaluation step: the evaluated set! statement.</param>
        /// <param name="resEnv">The new environment.</param>
        /// <returns>The next step to execute.</returns>
        private Stepper EvalSet(object args, Environment env, out object res, out Environment resEnv)
        {
            resEnv = env;
            object temp = this.interp.Eval(Second(args), env);
            res = env.Set(First(args), temp);
            return this.parent;
        }

        /// <summary>
        /// Evaluate an if expression.
        /// </summary>
        /// <param name="args">The body of the if.</param>
        /// <param name="env">The evaluation environment.</param>
        /// <param name="res">The result of the evaluation step: the evaluated body: the 
        ///     second or third clauses (depending on the value of the first).</param>
        /// <param name="resEnv">The new environment.</param>
        /// <returns>The next step to execute.</returns>
        private Stepper EvalIf(object args, Environment env, out object res, out Environment resEnv)
        {
            resEnv = env;
            object temp = this.interp.Eval(First(args), env);
            res = Truth(temp) ? Second(args) : Third(args);
            return this.parent;
        }

        /// <summary>
        /// Evaluate a sequence of objects, returning the last.
        /// </summary>
        /// <param name="args">A list of items to evaluate.</param>
        /// <param name="env">The evaluation environment.</param>
        /// <param name="res">The evaluation of the last expression..</param>
        /// <param name="resEnv">The resulting environment.</param>
        /// <returns>The next step.</returns>
        private Stepper EvalSequence(object args, Environment env, out object res, out Environment resEnv)
        {
            resEnv = env;
            while (Rest(args) != null)
            {
                this.interp.Eval(First(args), env);
                args = Rest(args);
            }

            res = First(args);
            return this.parent;
        }

        /// <summary>
        /// Evaluate the items in a list, given the environment.
        /// This is done to the args of a procedure call (except for special forms).
        /// This is an iterative, rather than a recursive one.
        /// </summary>
        /// <param name="list">The list of items to evaluate.</param>
        /// <param name="env">The evaluation environment.</param>
        /// <returns>A list of the evaluated items.</returns>
        private Pair EvalList(object list, Environment env)
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
        /// <param name="env">The environment in which they are evaluated</param>
        /// <returns>Another expression to evaluate instead.</returns>
        private object ReduceCond(object clauses, Environment env)
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
        /// <param name="env">End evaluation environment.</param>
        /// <returns>The result of the application.</returns>
        private object ApplyProc(object fn, object args, Environment env)
        {
            return Procedure.Proc(fn).Apply(this.interp, this.EvalList(args, env));
        }
    }
}
