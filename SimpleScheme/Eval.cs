#define OLDIFxx
// <copyright file="Eval.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    // TODO pull individual steppers out of Evaluator
    // TODO make evaluator the base class -- give better isolation
    using System;

    /// <summary>
    /// Template for an evaluation procedure.
    /// </summary>
    /// <returns>The stepper to execute next.</returns>
    public delegate Stepper Stepper();

    /// <summary>
    /// Evaluates expressions step by step.
    /// </summary>
    public abstract class Evaluator : SchemeUtils
    {
        /// <summary>
        /// The scheme interpreter.
        /// </summary>
        protected readonly Scheme interp;

        /// <summary>
        /// The parent evaluator.
        /// Return to this one when done.
        /// </summary>
        protected readonly Stepper parent;

        /// <summary>
        /// The expression being evaluated.  During evaluation, this is 
        ///   replaced by the results of evaluation.  At the conclusion of evaluation
        ///   this holds the final result.
        /// </summary>
        protected object expr;

        /// <summary>
        /// The evaluation environment.  If the environment is changes as a result
        ///   of evaluation, then the new environment is here after evaluation.
        /// </summary>
        protected Environment env;

        /// <summary>
        /// A program oounter.
        /// </summary>
        protected int pc;

        /// <summary>
        /// The function that we called.
        /// Valid when pc > 0.
        /// </summary>
        protected Evaluator called;

        /// <summary>
        /// Initializes a new instance of the Evaluator class.
        /// </summary>
        /// <param name="interp">The scheme interpreter.</param>
        /// <param name="parent">The parent evaluator.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluator environment.</param>
        protected Evaluator(Scheme interp, Stepper parent, object expr, Environment env)
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
        public object Expr
        {
            get { return this.expr; }
        }

        /// <summary>
        /// Gets the evaluation environment.  After execution, this is the new environment.
        /// </summary>
        public Environment Env
        {
            get { return this.env; }
        }

        public int Pc
        {
            get { return this.pc; }
        }

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

        public abstract Stepper EvalStep();
    }

    public class EvaluatorReturn : Evaluator
    {
        public EvaluatorReturn()
            : base(null, null, null, null)
        {
        }

        public override Stepper EvalStep()
        {
            throw new Exception("Do not call RvaluatorReturn::EvalStep");
        }
    }

        public class EvaluatorMain : Evaluator
    {
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

            if (this.pc == 101)
            {
                this.expr = this.called.Expr;
                this.env = this.called.Env;
                return this.parent;
            }

            if (this.pc == 1)
            {
                this.expr = this.called.Expr;
                this.env = this.called.Env;
                this.pc = 0;
            }

            if (this.expr is string)
            {
                // Evaluate a string by looking it up in the environment.
                // It should correspond to a varialbe name, for which there 
                //    is a corresponding value.
                this.expr = this.env.Lookup((string)this.expr);
                return this.parent;
            }

            if (!(this.expr is Pair))
            {
                // If we are evaluating something that is not a pair, 
                //    it must be a constant.
                // Return the integer, real, boolean, or vector.
                return this.parent;
            }

            // We are evaluating a pair.
            // Split out the first item for special treatment.
            object fn = First(this.expr);
            object args = Rest(this.expr);

            // Look for one of the special forms. 
            switch (fn as string)
            {
                case "quote":
                    // Evaluate quoted expression by just returning the expression.
                    this.expr = First(args);
                    return this.parent;

                case "begin":
                    {
                        // Evaluate begin by evaluating all the items in order, 
                        //   and returning the last.
                        this.called = new EvaluatorSequence(this.interp, this.EvalStep, args, this.env);
                        pc = 1;
                        return this.called.EvalStep();
                    }

                case "define":
                    {
                        // Define is a shortcut for lambda.
                        // Evaluate by splicing lambda on the front and evaluating that.
                        this.called = new EvaluatorDefine(this.interp, this.EvalStep, args, this.env);
                        pc = 101;
                        return this.called.EvalStep;
                    }

                case "set!":
                    {
                        // Evaluate a set! expression by evaluating the second, 
                        //   then setting the first to it.
                        this.called = new EvaluatorSet(this.interp, this.EvalStep, args, this.env);
                        this.pc = 101;       // returns to this class, stores result, then returns
                        return this.called.EvalStep;
                    }

                case "if":
                    {
                        // Eval an if expression by evaluating the first clause, 
                        //    and then returning either the second or third.
#if OLDIF
                        this.called = new EvaluatorIf(this.interp, this.EvalStep, args, this.env);
                        this.pc = 1;       // continues after the if, with the returned expr
                        this.called.EvalStep();
                        return this.EvalStep;
#else
                        this.called = new EvaluatorIf(this.interp, this.EvalStep, args, this.env);
                        pc = 1;
                        return called.EvalStep;
#endif
                    }

                case "cond":
                    this.expr = this.ReduceCond(args);
                    called = this;
                    pc = 1;
                    return this.EvalStep;

                case "lambda":
                    // Evaluate a lambda by creating a closure.
                    this.expr = new Closure(First(args), Rest(args), this.env);
                    called = this;
                    pc = 101;
                    return this.EvalStep;

                case "macro":
                    // Evaluate a macro by creating a macro.
                    this.expr = new Macro(First(args), Rest(args), this.env);
                    called = this;
                    pc = 101;
                    return this.EvalStep;
            }

            // If we get here, it wasn't one of the special forms.  
            // So we need to evaluate the first item (the function) in preparation for
            //    doing a procedure call.
            fn = this.interp.Eval(fn, this.env);

            // If the function is a macro, expand it and then continue.
            if (fn is Macro)
            {
                Macro m = (Macro)fn;
                this.expr = m.Expand(this.interp, (Pair) this.expr, args);
                called = this;
                pc = 1;
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
                this.expr = f.Body;
                this.env = new Environment(f.Parms, this.EvalList(args), f.Env);
                called = this;
                pc = 1;
                return this.EvalStep;
            }

            // This is a procedure call.
            // In any other case, the function is a primitive or a user-defined function.
            // Evaluate the arguments in the environment, then apply the function 
            //    to the arguments.
            this.expr = this.ApplyProc(fn, args);
            called = this;
            pc = 101;
            return this.EvalStep;
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
                object value = this.interp.Eval(First(list), this.env);
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
                    result = this.interp.Eval(First(clause), this.env);
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

    public class EvaluatorSet : Evaluator
    {
        private object temp;

        public EvaluatorSet(Scheme interp, Stepper parent, object expr, Environment env) 
            : base(interp, parent, expr, env) { }

        /// <summary>
        /// Evaluate a set! expression.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        public override Stepper EvalStep()
        {
            switch (this.pc)
            {
                case 0:
                    this.temp = this.interp.Eval(Second(this.expr), this.env);
                    this.pc = 1;
                    return this.EvalStep;
                case 1:
                    this.expr = this.env.Set(First(this.expr), this.temp);
                    break;
            }

            return this.parent;
        }
        
    }

    public class EvaluatorDefine : Evaluator
    {
        private object temp;

        public EvaluatorDefine(Scheme interp, Stepper parent, object expr, Environment env) 
            : base(interp, parent, expr, env) { }

        /// <summary>
        /// Evaluate a define expression.
        /// Handle the define syntax shortcut by inserting lambda.
        /// </summary>
        /// <returns>The evaluated definition.</returns>
        public override Stepper EvalStep()
        {
            switch (this.pc)
            {
                case 0:
                    {
                        if (First(this.expr) is Pair)
                        {
                            this.temp = this.interp.Eval(Cons("lambda", Cons(Rest(First(this.expr)), Rest(this.expr))), this.env);
                            this.pc = 1;
                        }
                        else
                        {
                            this.temp = this.interp.Eval(Second(this.expr), this.env);
                            this.pc = 2;
                        }
                    }

                    return this.EvalStep;
                case 1:
                    this.expr = this.env.Define(First(First(this.expr)), this.temp);
                    break;
                case 2:
                     this.expr = this.env.Define(First(this.expr),  this.temp);
                    break;
            }

            return this.parent;
        }
        
    }
#if OLDIF
    public class EvaluatorIf : Evaluator
    {
        public EvaluatorIf(Scheme interp, Stepper parent, object expr, Environment env) 
            : base(interp, parent, expr, env) { }

        public override Stepper EvalStep()
        {
            object value = this.interp.Eval(First(this.expr), this.env);
 Console.WriteLine("EvalIf {0} {1}", this.expr, value);
            this.expr = Truth(value) ? Second(this.expr) : Third(this.expr);
Console.WriteLine("EvalIf result: {0}", this.expr);
            return this.parent;
        }
    }
#else
    public class EvaluatorIf : Evaluator
    {
        private object temp;

        public EvaluatorIf(Scheme interp, Stepper parent, object expr, Environment env) 
            : base(interp, parent, expr, env) { }

        /// <summary>
        /// Evaluate an if expression.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        public override Stepper EvalStep()
        {
            switch (this.pc)
            {
                case 0:
                    this.temp = this.interp.Eval(First(this.expr), this.env);
 Console.WriteLine("EvalIf 1 {0} {1}", this.expr, this.temp);
                    this.pc = 1;
                    return this.EvalStep;
                case 1:
 Console.WriteLine("EvalIf 2 {0} {1}", this.expr, this.temp);
                    expr = Truth(this.temp) ? Second(this.expr) : Third(this.expr);
Console.WriteLine("EvalIf result: {0}", expr);
                    break;
            }
            return this.parent;
        }
    }
#endif        

    public class EvaluatorSequence : Evaluator
    {
        public EvaluatorSequence(Scheme interp, Stepper parent, object expr, Environment env) 
            : base(interp, parent, expr, env) { }

        /// <summary>
        /// Evaluate a sequence of objects, returning the last.
        /// </summary>
        /// <returns>The next step.</returns>
        public override Stepper EvalStep()
        {
            while (Rest(this.expr) != null)
            {
                this.interp.Eval(First(this.expr), this.env);
                this.expr = Rest(this.expr);
            }

            this.expr = First(this.expr);
            return this.parent;
        }

        
    }
}
