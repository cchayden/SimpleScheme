// <copyright file="Eval.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// The part of the Scheme class that has to do with evaluation.
    /// </summary>
    public sealed partial class Scheme
    {
        /// <summary>
        /// Evaluate an expression in the given environment.
        /// Besides all the special forms, the main action is to:
        ///   return strings and constants, which evaluate to themselves
        ///   treat the first arg as a procedure, evaluate the rest of the expression args, 
        ///   and apply the procedure to the evaluated results.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to evaluate it in.</param>
        /// <returns>The result of the evaluation.</returns>
        public object Eval(object expr, Environment env)
        {
            while (true)
            {
                if (expr is string)
                {
                    // Evaluate a string by looking it up in the environment.
                    // It should correspond to a varialbe name, for which there 
                    //    is a corresponding value.
                    return env.Lookup((string)expr);
                }

                if (!(expr is Pair))
                {
                    // If we are evaluating something that is not a pair, 
                    //    it must be a constant.
                    // Return the integer, real, boolean, or vector.
                    return expr;
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
                        return First(args);

                    case "begin":
                        // Evaluate begin by evaluating all the items in order, 
                        //   and returning the last.
                        expr = this.EvalSequence(args, env);
                        continue;
                    case "define":
                        // Define is a shortcut for lambda.
                        // Evaluate by splicing lambda on the front and evaluating that.
                        if (First(args) is Pair)
                        {
                            return env.Define(
                                First(First(args)),
                                this.Eval(Cons("lambda", Cons(Rest(First(args)), Rest(args))), env));
                        }

                        return env.Define(First(args), this.Eval(Second(args), env));
                    case "set!":
                        // Evaluate a set! expression by evaluating the second, 
                        //   then setting the first to it.
                        return env.Set(First(args), this.Eval(Second(args), env));
                    case "if":
                        // Eval an if expression by evaluating the first clause, 
                        //    and then returning either the second or third.
                        expr = Truth(this.Eval(First(args), env)) ? Second(args) : Third(args);
                        continue;
                    case "cond":
                        expr = this.ReduceCond(args, env);
                        continue;
                    case "lambda":
                        // Evaluate a lambda by creating a closure.
                        return new Closure(First(args), Rest(args), env);
                    case "macro":
                        // Evaluate a macro by creating a macro.
                        return new Macro(First(args), Rest(args), env);
                }

                // If we get here, it wasn't one of the special forms.  
                // So we need to evaluate the first item (the function) in preparation for
                //    doing a procedure call.
                fn = this.Eval(fn, env);

                // If the function is a macro, expand it and then continue.
                if (fn is Macro)
                {
                    Macro m = (Macro)fn;
                    expr = m.Expand(this, (Pair)expr, args);
                    continue;
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
                    //env = new Environment(f.Parms, this.EvalList(args, env), f.Env);
                    continue;
                }

                // This is a procedure call.
                // In any other case, the function is a primitive or a user-defined function.
                // Evaluate the arguments in the environment, then apply the function 
                //    to the arguments.
                return Procedure.Proc(fn).Apply(this, this.EvalList(args, env));
            }
        }

        /// <summary>
        /// Evaluate a sequence of objects, returning the last.
        /// </summary>
        /// <param name="args">A list of items to evaluate.</param>
        /// <param name="env">The evaluation environment.</param>
        /// <returns>The evaluation of the last</returns>
        private object EvalSequence(object args, Environment env)
        {
            while (Rest(args) != null)
            {
                this.Eval(First(args), env);
                args = Rest(args);
            }

            return First(args);
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
                object value = this.Eval(First(list), env);
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
                if (First(clause) as string == "else" || Truth(result = this.Eval(First(clause), env)))
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
    }
}
