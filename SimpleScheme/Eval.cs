#define OLDxx
// <copyright file="Eval.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
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
            this.Interp = interp;
            this.Parent = parent;
            this.RetExpr = this.Expr = expr;
            this.RetEnv = this.Env = env;
            this.Pc = 0;
            this.Called = null;
        }

        protected Scheme Interp { get; private set; }
        protected Evaluator Parent { get; private set; }

        /// <summary>
        /// Gets or sets the expression being evaluated.  
        /// </summary>
        public object Expr { get; protected set; }

        /// <summary>
        /// Gets or sets the returned expression.
        /// This is valid after a call has completed, and holds the
        ///   returned result.
        /// </summary>
        public object RetExpr { get; protected set; }

        /// <summary>
        /// Gets or sets the evaluation environment.  After execution, this is the new environment.
        /// </summary>
        public Environment Env { get; protected set; }

        /// <summary>
        /// Gets or sets the returned environment
        /// </summary>
        public Environment RetEnv { get; protected set; }

        /// <summary>
        /// Gets or sets the Evaluators program counter.
        /// Used to sequence through multiple steps.
        /// </summary>
        protected int Pc { get; set;  }

        /// <summary>
        /// Gets or sets the called sub-evaluator.
        /// The evaluator that is called is stored, so that the returned value
        ///   can be extracted after it returns.
        /// </summary>
        protected Evaluator Called { get; set; }

        /// <summary>
        /// Subclasses implement this to make one step in the evaluation.
        /// </summary>
        /// <returns></returns>
        public abstract Evaluator EvalStep();

        /// <summary>
        /// Make a call to a sub-evaluator.
        /// When that returns, store the result but do not return.
        /// </summary>
        /// <param name="toCall">The evaluator to call.</param>
        /// <returns>The first step of the sub-evaluator.</returns>
        protected Evaluator SubEval(Evaluator toCall)
        {
            this.Called = toCall;
            return toCall;
        }

        /// <summary>
        /// Make a call to a sub-evaluator.
        /// When that returns, store the result and return.
        /// </summary>
        /// <param name="toCall">The evaluator to call.</param>
        /// <returns>The first step of the sub-evaluator.</returns>
        protected Evaluator SubEvalReturn(Evaluator toCall)
        {
            Pc = PcReturn;
            return SubEval(toCall);
        }

        /// <summary>
        /// Return immediately.
        /// </summary>
        /// <returns>The parent evaluator..</returns>
        protected Evaluator EvalReturn()
        {
            return this.Parent;
        }

        /// <summary>
        /// Continue executing in the existing evaluator.
        /// </summary>
        /// <returns></returns>
        protected Evaluator EvalContinue()
        {
            Called = null;
            return this;
        }

        /// <summary>
        /// Recursive Evaluator call.
        /// </summary>
        /// <param name="args">The expression to evaluate.</param>
        /// <returns>The next evaluation step.</returns>
        protected Evaluator CallEval(object args)
        {
            return SubEval(new EvaluatorMain(this.Interp, this, args, this.Env));
        }
    }

    /// <summary>
    /// This evaluator is used to signal the end of evaulation.
    /// This is the evaluator that the top level evaulator "returns" to.
    /// </summary>
    public class EvaluatorReturn : Evaluator
    {
        /// <summary>
        /// Initializes a new instance of the EvaluatorReturn class.
        /// </summary>
        public EvaluatorReturn()
            : base(null, null, null, null)
        {
        }

        /// <summary>
        /// This evaluator should never execute steps.
        /// </summary>
        /// <returns>Throws an exception -- does not return.</returns>
        public override Evaluator EvalStep()
        {
            throw new Exception("Do not call RvaluatorReturn::EvalStep");
        }
    }

    /// <summary>
    /// The main evaluator for expressions.
    /// </summary>
    public class EvaluatorMain : Evaluator
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
            // pick up results of call if returning or on first step
            if (this.Called != null && Pc < 1)
            {
                this.Expr = this.Called.RetExpr;
                this.Env = this.Called.RetEnv;
            }

            switch (this.Pc)
            {
                case PcReturn:
                    this.RetExpr = this.Expr;
                    return EvalReturn();

                case 0:
                    if (this.Expr is string)
                    {
                        // Evaluate a string by looking it up in the environment.
                        // It should correspond to a varialbe name, for which there 
                        //    is a corresponding value.
                        this.RetExpr = this.Env.Lookup((string) this.Expr);
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
                            return SubEval(new EvaluatorReduceCond(this.Interp, this, args, this.Env));

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
                    Pc = 1;
                    return CallEval(fn);

               case 1:
                    fn = Called.RetExpr;

                    // If the function is a macro, expand it and then continue.
                    if (fn is Macro)
                    {
                        Macro m = (Macro) fn;
                        this.Expr = m.Expand(this.Interp, (Pair) this.Expr, args);
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
                        Closure f = (Closure) fn;
                        SubEvalReturn(new EvaluatorClosure(Interp, this, args, Env, f));
                    }

                    // This is a procedure call.
                    // In any other case, the function is a primitive or a user-defined function.
                    // Evaluate the arguments in the environment, then apply the function 
                    //    to the arguments.
                    return SubEvalReturn(new EvaluatorApplyProc(Interp, this, args, Env, fn));
            }
            throw new Exception("EvaluatorMain pc error");
        }
    }

    public class EvaluatorSet : Evaluator
    {
        private object first;

        /// <summary>
        /// Initializes a new instance of the EvaluatorSet class.
        /// </summary>
        /// <param name="interp">The interpreter.</param>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        public EvaluatorSet(Scheme interp, Evaluator parent, object expr, Environment env) 
            : base(interp, parent, expr, env) { }

        /// <summary>
        /// Evaluate a set! expression.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        public override Evaluator EvalStep()
        {
            switch(Pc)
            {
                case 0:
                    Pc = 1;
                    first = First(this.Expr);
                    return CallEval(Second(this.Expr));
                case 1:
                    this.RetExpr = this.Env.Set(first, Called.RetExpr);
                    break;
            }
            return EvalReturn();
        }
        
    }

    /// <summary>
    /// Evaluate a define expression.
    /// </summary>
    public class EvaluatorDefine : Evaluator
    {
        /// <summary>
        /// Initializes a new instance of the EvaluatorDefine class.
        /// </summary>
        /// <param name="interp">The interpreter.</param>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        public EvaluatorDefine(Scheme interp, Evaluator parent, object expr, Environment env) 
            : base(interp, parent, expr, env) { }

        /// <summary>
        /// Evaluate a define expression.
        /// Handle the define syntax shortcut by inserting lambda.
        /// </summary>
        /// <returns>The evaluated definition.</returns>
        public override Evaluator EvalStep()
        {
            switch (Pc)
            {
                case 0:
                    if (First(this.Expr) is Pair)
                    {
                        Pc = 1;
                        return CallEval(Cons("lambda", Cons(Rest(First(this.Expr)), Rest(this.Expr))));
                    }
                    Pc = 2;
                    return CallEval(Second(this.Expr));
                case 1:
                    this.RetExpr = this.Env.Define(First(First(this.Expr)), Called.RetExpr);
                    break;
                case 2:
                    this.RetExpr = this.Env.Define(First(this.Expr), Called.RetExpr);
                    break;
            }
            return EvalReturn();
        }
    }

    /// <summary>
    /// Evaluate an if expression.
    /// Evaluate the first part, then depending on its truth value, either
    ///   evaluate the second or third part.
    /// </summary>
    public class EvaluatorIf : Evaluator
    {
        /// <summary>
        /// Initializes a new instance of the EvaluatorIf class.
        /// </summary>
        /// <param name="interp">The interpreter.</param>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        public EvaluatorIf(Scheme interp, Evaluator parent, object expr, Environment env) 
            : base(interp, parent, expr, env) { }

        /// <summary>
        /// Evaluate an if expression.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        public override Evaluator EvalStep()
        {
            switch (Pc)
            {
                case 0:
                    Pc = 1;
                    return CallEval(First(this.Expr));
                case 1:
                    this.RetExpr = Truth(Called.RetExpr) ? Second(this.Expr) : Third(this.Expr);
                    break;
            }
            return EvalReturn();
        }
    }

    public class EvaluatorSequence : Evaluator
    {
        /// <summary>
        /// Initializes a new instance of the EvaluatorSequence class.
        /// </summary>
        /// <param name="interp">The interpreter.</param>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        public EvaluatorSequence(Scheme interp, Evaluator parent, object expr, Environment env)
            : base(interp, parent, expr, env) { }

        /// <summary>
        /// Evaluate a sequence of objects, returning the last.
        /// This was a simple while loop that has been split in the middle.
        /// </summary>
        /// <returns>The next step.</returns>
        public override Evaluator EvalStep()
        {
            switch (this.Pc)
            {
                case 0:
                    if (Rest(this.Expr) != null)
                    {
                        this.Pc = 1;
                        return CallEval(First(this.Expr));
                    }
                    this.RetExpr = First(this.Expr);
                    break;
                case 1:
                    this.Expr = Rest(this.Expr);
                    this.Pc = 0;
                    return EvalContinue();
            }
            return EvalReturn();
        }
    }

    /// <summary>
    /// Evaluate the items in a list, given the environment.
    /// This is done to the args of a procedure call (except for special forms).
    /// This is an iterative, rather than a recursive one.
    /// </summary>
    public class EvaluatorList : Evaluator
    {
        private Pair result;
        private Pair accum;

        /// <summary>
        /// Initializes a new instance of the EvaluatorList class.
        /// </summary>
        /// <param name="interp">The interpreter.</param>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        public EvaluatorList(Scheme interp, Evaluator parent, object expr, Environment env)
            : base(interp, parent, expr, env) { }

        /// <summary>
        /// Evaluate a list of expressions.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        public override Evaluator EvalStep()
        {
            switch (this.Pc)
            {
                case 0:
                    // first check for degenerate cases
                    if (this.Expr == null)
                    {
                        this.RetExpr = null;
                        break;
                    }

                    if (!(this.Expr is Pair))
                    {
                        Error("Illegal arg list: " + this.Expr);
                        this.RetExpr = null;
                        break;
                    }
                    // start with an empty list
                    accum = result = List(null); // empty cell will be stripped off below
                    Pc = 1;
                    return EvalContinue();
                case 1:
                    if (this.Expr is Pair)
                    {
                        // if there is more to do, evaluate the first expression
                        Pc = 2;
                        return CallEval(First(this.Expr));
                    }
                    // if we are done, just return the result minus the dummy entry
                    this.RetExpr = result.Rest;
                    break;
                case 2:
                    // back from the evaluation -- save the result and keep going with the rest
                    Pc = 1;
                    accum = (Pair) (accum.Rest = List(Called.RetExpr));
                    Expr = Rest(Expr);
                    return EvalContinue();
            }
            return EvalReturn();
        }
    }

    /// <summary>
    /// Evaluate a closure
    /// </summary>
    public class EvaluatorClosure : Evaluator
    {
        private readonly Closure f;
        
        /// <summary>
        /// Initializes a new instance of the EvaluatorSet class.
        /// </summary>
        /// <param name="interp">The interpreter.</param>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="f">The closure to evaluate</param>
        public EvaluatorClosure(Scheme interp, Evaluator parent, object expr, Environment env, Closure f) 
            : base(interp, parent, expr, env)
        {
            this.f = f;
            this.RetExpr = f.Body;
        }

        /// <summary>
        /// Evaluate a set! expression.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        public override Evaluator EvalStep()
        {
            switch (this.Pc)
            {
                case 0:
                    Pc = 1;
                    Called = new EvaluatorList(Interp, this, Expr, Env);
                    return Called;
                case 1:
                    this.RetEnv = new Environment(f.Parms, Called.RetExpr, f.Env);
                    break;
            }
            return EvalReturn();
        }
        
    }

    /// <summary>
    /// Evaluate args and apply a proc to it.
    /// </summary>
    public class EvaluatorApplyProc : Evaluator
    {
        private readonly object fn;
        
        /// <summary>
        /// Initializes a new instance of the EvaluatorApplyProc class.
        /// </summary>
        /// <param name="interp">The interpreter.</param>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="fn">The function to apply.</param>
        public EvaluatorApplyProc(Scheme interp, Evaluator parent, object expr, Environment env, object fn) 
            : base(interp, parent, expr, env)
        {
            this.fn = fn;
        }

        /// <summary>
        /// Evaluate a proc application.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        public override Evaluator EvalStep()
        {
            switch (this.Pc)
            {
                case 0:
                    Pc = 1;
                    Called = new EvaluatorList(Interp, this, Expr, Env);
                    return Called;
                case 1:
                    this.RetExpr = Procedure.Proc(fn).Apply(this.Interp, Called.RetExpr);
                    break;
            }
            return EvalReturn();
        }
        
    }

    /// <summary>
    /// Reduce a conditiona;
    /// </summary>
    public class EvaluatorReduceCond : Evaluator
    {
        private object result;
        private object clause;
        
        /// <summary>
        /// Initializes a new instance of the EvaluatorSet class.
        /// </summary>
        /// <param name="interp">The interpreter.</param>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        public EvaluatorReduceCond(Scheme interp, Evaluator parent, object expr, Environment env) 
            : base(interp, parent, expr, env)
        { }

        /// <summary>
        /// Handle a cond by iterating down the list of clauses.
        /// The clauses are (guard expression) pairs.
        /// If they are exhausted, return False.
        /// If we find a True guard or an else, then:
        ///     If the clause has no expression, return the guard.
        ///     Otherwise return the expression to be evaluated.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        public override Evaluator EvalStep()
        {
            switch (this.Pc)
            {
                case 0:
                    if (Expr == null)
                    {
                        RetExpr = False;
                        return EvalReturn();
                    }
                    clause = First(Expr);
                    Expr = Rest(Expr);
                    if (First(clause) as string == "else")
                    {
                        result = null;
                        Pc = 2;
                    }
                    else
                    {
                        Pc = 1;
                        return CallEval(First(clause));
                    }
                    return EvalContinue();
                case 1:
                    result = Called.RetExpr;
                    Pc = Truth(result) ? 2 : 0;
                    return EvalContinue();
                case 2:
                    if (Rest(clause) == null)
                    {
                        RetExpr =  List("quote", result);
                        break;
                    }

                    if (Second(clause) as string == "=>")
                    {
                        RetExpr = List(Third(clause), List("quote", result));
                        break;
                    }

                    RetExpr = Cons("begin", Rest(clause));
                    break;
            }
            return EvalReturn();
        }
    }
}
