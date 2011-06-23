// <copyright file="Eval.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    // TODO pull individual Evaluators out of Evaluator
    // TODO make evaluator the base class -- give better isolation
    using System;

    /// <summary>
    /// Template for an evaluation procedure.
    /// </summary>
    /// <returns>The Evaluator to execute next.</returns>
    //public delegate Evaluator Evaluator();

    /// <summary>
    /// Evaluates expressions step by step.
    /// </summary>
    public abstract class Evaluator : SchemeUtils
    {
        /// <summary>
        /// The scheme interpreter.
        /// </summary>
        private readonly Scheme interp;

        /// <summary>
        /// The parent evaluator.
        /// Return to this one when done.
        /// </summary>
        private readonly Evaluator parent;

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
        /// The function that we called.
        /// Valid when pc > 0.
        /// </summary>
        private Evaluator called;

        protected const int PcReturn = -1;

        /// <summary>
        /// Initializes a new instance of the Evaluator class.
        /// </summary>
        /// <param name="interp">The scheme interpreter.</param>
        /// <param name="parent">The parent evaluator.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluator environment.</param>
        protected Evaluator(Scheme interp, Evaluator parent, object expr, Environment env)
        {
            this.interp = interp;
            this.parent = parent;
            this.expr = expr;
            this.env = env;
            this.pc = 0;
            this.called = null;
        }

        protected Scheme Interp { get { return interp; } }
        protected Evaluator Parent { get { return parent; } }

        /// <summary>
        /// Gets the expression being evaluated.  Aftet execution is done, this is the
        ///     evaluation result.
        /// </summary>
        public object Expr
        {
            get { return this.expr; }
            set { expr = value; }
        }

        /// <summary>
        /// Gets the evaluation environment.  After execution, this is the new environment.
        /// </summary>
        public Environment Env
        {
            get { return this.env; }
            set { env = value; }
        }

        protected int Pc
        {
            get { return this.pc; }
            set { pc = value; }
        }

        protected Evaluator Called 
        { 
            get { return called; }
            set { called = value; }
        }

        public abstract Evaluator EvalStep();

        /// <summary>
        /// Make a call to a sub-evaluator.
        /// When that returns, store the result and return.
        /// </summary>
        /// <param name="toCall">The evaluator to call.</param>
        /// <returns>The first step of the sub-evaluator.</returns>
        protected Evaluator SubEvalReturn(Evaluator toCall)
        {
            this.called = toCall;
            pc = PcReturn;
            return toCall;
        }

        /// <summary>
        /// Make a call to a sub-evaluator.
        /// When that returns, store the result but do not return.
        /// </summary>
        /// <param name="toCall">The evaluator to call.</param>
        /// <returns>The first step of the sub-evaluator.</returns>
        protected Evaluator SubEval(Evaluator toCall)
        {
            this.called = toCall;
            return toCall;
        }

        /// <summary>
        /// Store the result and return.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected Evaluator EvalReturn()
        {
            return SubEvalReturn(this);
        }

        /// <summary>
        /// Continue executing in the existing evaluator.
        /// </summary>
        /// <returns></returns>
        protected Evaluator EvalContinue()
        {
            called = this;
            return this;
        }
    }

    public class EvaluatorReturn : Evaluator
    {
        public EvaluatorReturn()
            : base(null, null, null, null)
        {
        }

        public override Evaluator EvalStep()
        {
            throw new Exception("Do not call RvaluatorReturn::EvalStep");
        }
    }

        public class EvaluatorMain : Evaluator
    {
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
            if (this.Called != null)
            {
                this.Expr = this.Called.Expr;
                this.Env = this.Called.Env;
            }

            if (this.Pc == PcReturn)
            {
                return this.Parent;
            }

            if (this.Expr is string)
            {
                // Evaluate a string by looking it up in the environment.
                // It should correspond to a varialbe name, for which there 
                //    is a corresponding value.
                this.Expr = this.Env.Lookup((string)this.Expr);
                return EvalReturn();
            }

            if (!(this.Expr is Pair))
            {
                // If we are evaluating something that is not a pair, 
                //    it must be a constant.
                // Return the integer, real, boolean, or vector.
                return EvalReturn();
            }

            // We are evaluating a pair.
            // Split out the first item for special treatment.
            object fn = First(this.Expr);
            object args = Rest(this.Expr);

            // Look for one of the special forms. 
            switch (fn as string)
            {
                case "quote":
                    // Evaluate quoted expression by just returning the expression.
                    this.Expr = First(args);
                    return EvalReturn();

                case "begin":
                    {
                        // Evaluate begin by evaluating all the items in order, 
                        //   and returning the last.
                        return SubEval(new EvaluatorSequence(this.Interp, this, args, this.Env));
                    }

                case "define":
                    {
                        // Define is a shortcut for lambda.
                        // Evaluate by splicing lambda on the front and evaluating that.
                        return SubEvalReturn(new EvaluatorDefine(this.Interp, this, args, this.Env));
                    }

                case "set!":
                    {
                        // Evaluate a set! expression by evaluating the second, 
                        //   then setting the first to it.
                        return SubEvalReturn(new EvaluatorSet(this.Interp, this, args, this.Env));
                    }

                case "if":
                    {
                        // Eval an if expression by evaluating the first clause, 
                        //    and then returning either the second or third.
                        return SubEval(new EvaluatorIf(this.Interp, this, args, this.Env));
                    }

                case "cond":
                    this.Expr = this.ReduceCond(args);
                    return EvalContinue();

                case "lambda":
                    // Evaluate a lambda by creating a closure.
                    this.Expr = new Closure(First(args), Rest(args), this.Env);
                    return EvalReturn();

                case "macro":
                    // Evaluate a macro by creating a macro.
                    this.Expr = new Macro(First(args), Rest(args), this.Env);
                    return EvalReturn();
            }

            // If we get here, it wasn't one of the special forms.  
            // So we need to evaluate the first item (the function) in preparation for
            //    doing a procedure call.
            fn = this.Interp.Eval(fn, this.Env);

            // If the function is a macro, expand it and then continue.
            if (fn is Macro)
            {
                Macro m = (Macro)fn;
                this.Expr = m.Expand(this.Interp, (Pair) this.Expr, args);
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
                this.Expr = f.Body;
                this.Env = new Environment(f.Parms, this.EvalList(args), f.Env);
                return EvalContinue();
            }

            // This is a procedure call.
            // In any other case, the function is a primitive or a user-defined function.
            // Evaluate the arguments in the environment, then apply the function 
            //    to the arguments.
            this.Expr = this.ApplyProc(fn, args);
            return EvalReturn();
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
                object value = this.Interp.Eval(First(list), this.Env);
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
                    result = this.Interp.Eval(First(clause), this.Env);
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
            return Procedure.Proc(fn).Apply(this.Interp, this.EvalList(args));
        }
    }

    public class EvaluatorSet : Evaluator
    {
        private object temp;

        public EvaluatorSet(Scheme interp, Evaluator parent, object expr, Environment env) 
            : base(interp, parent, expr, env) { }

        /// <summary>
        /// Evaluate a set! expression.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        public override Evaluator EvalStep()
        {
            switch (this.Pc)
            {
                case 0:
                    this.temp = this.Interp.Eval(Second(this.Expr), this.Env);
                    this.Pc = 1;
                    return this;
                case 1:
                    this.Expr = this.Env.Set(First(this.Expr), this.temp);
                    break;
            }

            return this.Parent;
        }
        
    }

    public class EvaluatorDefine : Evaluator
    {
        private object temp;

        public EvaluatorDefine(Scheme interp, Evaluator parent, object expr, Environment env) 
            : base(interp, parent, expr, env) { }

        /// <summary>
        /// Evaluate a define expression.
        /// Handle the define syntax shortcut by inserting lambda.
        /// </summary>
        /// <returns>The evaluated definition.</returns>
        public override Evaluator EvalStep()
        {
            switch (this.Pc)
            {
                case 0:
                    {
                        if (First(this.Expr) is Pair)
                        {
                            this.temp = this.Interp.Eval(Cons("lambda", Cons(Rest(First(this.Expr)), Rest(this.Expr))), this.Env);
                            this.Pc = 1;
                        }
                        else
                        {
                            this.temp = this.Interp.Eval(Second(this.Expr), this.Env);
                            this.Pc = 2;
                        }
                    }

                    return this;
                case 1:
                    this.Expr = this.Env.Define(First(First(this.Expr)), this.temp);
                    break;
                case 2:
                     this.Expr = this.Env.Define(First(this.Expr),  this.temp);
                    break;
            }

            return this.Parent;
        }
        
    }
    public class EvaluatorIf : Evaluator
    {
        private object temp;

        public EvaluatorIf(Scheme interp, Evaluator parent, object expr, Environment env) 
            : base(interp, parent, expr, env) { }

        /// <summary>
        /// Evaluate an if expression.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        public override Evaluator EvalStep()
        {
            switch (this.Pc)
            {
                case 0:
                    this.temp = this.Interp.Eval(First(this.Expr), this.Env);
                    this.Pc = 1;
                    return this;
                case 1:
                    Expr = Truth(this.temp) ? Second(this.Expr) : Third(this.Expr);
                    break;
            }
            return this.Parent;
        }
    }

    public class EvaluatorSequence : Evaluator
    {
        public EvaluatorSequence(Scheme interp, Evaluator parent, object expr, Environment env) 
            : base(interp, parent, expr, env) { }

        /// <summary>
        /// Evaluate a sequence of objects, returning the last.
        /// </summary>
        /// <returns>The next step.</returns>
        public override Evaluator EvalStep()
        {
            if (Pc == 1)
            {
                this.Expr = Rest(this.Expr);
            }
            if (Rest(this.Expr) != null)
            {
                this.Pc = 1;
                this.Interp.Eval(First(this.Expr), this.Env);
                return this;
            }
            this.Expr = First(this.Expr);
            return this.Parent;
        }
    }
}
