// <copyright file="EvaluatorMain.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// The main evaluator for expressions.
    /// </summary>
   //// <r4rs section="4.1.3">(<operator> <operand1> ...)</r4rs>
    public sealed class EvaluatorMain : Stepper
    {
        /// <summary>
        /// Trace flag.
        /// </summary>
        private readonly bool trace;

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
            this.Pc = this.InitialStep;
            this.trace = false;
            IncrementCounter("eval");
        }

        /// <summary>
        /// Calls the main evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
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
        /// Evaluate a string by looking it up in the environment.
        /// </summary>
        /// <param name="symbol">The symbol</param>
        /// <param name="env">The environment to evaluate it in.</param>
        /// <returns>The value of the symbol.</returns>
        private static object EvalString(string symbol, Environment env)
        {
            return env.Lookup(symbol);
        }

        /// <summary>
        /// Start evaluation by testing the various forms.
        /// The special forms have their own evaluators.
        /// Otherwise, evaluate the first argument (the proc) in preparation for a call.
        /// </summary>
        /// <returns>The next thing to do.</returns>
        private Stepper InitialStep()
        {
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

            if (this.trace)
            {
                Console.WriteLine("{0} {1}", this.fn, this.args);
            }

            // defined as macro for now

            //// <r4rs section="4.2.4">(do ((variable1> <init> <step>) 
            //                           ...
            //                           (<test> <expression1> ...)
            //                         <command> ...)</r4rs>

            //// <r4rs section="4.2.5">(delay <expression>)</r4rs>

            //// <r4rs section="4.2.6">(quasiquote <template>)</r4rs>

            // Look for one of the special forms. 
            this.Pc = this.ReturnStep;
            switch (this.fn as string)
            {
                case "quote":
                    // Evaluate quoted expression by just returning the expression.
                    //// <r4rs section="4.1.2">(quote <datum>)</r4rs>
                    return ReturnFromStep(List.First(this.args));

                case "begin":
                    // Evaluate begin by evaluating all the items in order, 
                    //   and returning the last.
                    //// <r4rs section="4.2.3">(begin <expression1> <expression2> ...)</r4rs>
                    return EvaluateSequence.Call(this, this.args);

                case "define":
                    // Define is a shortcut for lambda.
                    // Evaluate by splicing lambda on the front and evaluating that.
                    //// <r4rs section="5.2">(define <variable> <expression>)</r4rs>
                    //// <r4rs section="5.2">(define (<variable> <formals>) <body>)</r4rs>
                    //// <r4rs section="5.2">(define (<variable> . <formal>) <body>)</r4rs>
                    //// <r4rs section="5.2">(begin <definition1> <definition2> ...)</r4rs>
                    return EvaluateDefine.Call(this, this.args);

                case "set!":
                    // Evaluate a set! expression by evaluating the second, 
                    //   then setting the first to it.
                    //// <r4rs section="4.1.6">(set <variable> <expression>)</r4rs>
                    return EvaluateSet.Call(this, this.args);

                case "if":
                    // Eval an if expression by evaluating the first clause, 
                    //    and then returning either the second or third.
                    //// <r4rs section="4.1.5">(if <test> <consequent> <alternate>)</r4rs>
                    //// <r4rs section="4.1.5">(if <test> <consequent>)</r4rs>
                    return EvaluateIf.Call(this, this.args);

                case "case":
                    return EvaluateCase.Call(this, this.args);

                case "or":
                    //// <r4rs section="4.2.1">(or <test1> ...)</r4rs>
                    return EvaluateOr.Call(this, this.args);

                case "and":
                    //// <r4rs section="4.2.1">(and <test1> ...)</r4rs>
                    return EvaluateAnd.Call(this, this.args);

                case "cond":
                    //// <r4rs section="4.1.5">(if <test> <consequent> <alternate>)<r4rs>
                    //// <r4rs section="4.2.1">(cond <clause1> <clause2> ... ()</r4rs>
                    //// <r4rs section="4.2.1">clause: (<test> <expression>)</r4rs>
                    //// <r4rs section="4.2.1">clause: (<test> => <recipient>)</r4rs>
                    //// <r4rs section="4.2.1">else clause: (else <expression1> <expression2> ...)</r4rs>
                    return EvaluateCond.Call(this, this.args);

                case "let":
                    //// <r4rs section="4.2.2">(let <bindings> <body>)</r4rs>
                    //// <r4rs section="4.2.4">(let <variable> <bindings> <body>)</r4rs>
                    //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
                    //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
                    return EvaluateLet.Call(this, this.args);

                case "let*":
                    //// <r4rs section="4.2.2">(let* <bindings> <body>)</r4rs>
                    //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
                    //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
                    return EvaluateLetStar.Call(this, this.args);

                case "letrec":
                    //// <r4rs section="4.2.2">(letrec <bindings> <body>)</r4rs>
                    //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
                    //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
                    return EvaluateLetRec.Call(this, this.args);

                case "do":
                    //// <r4rs section="4.2.4">(do ((variable1> <init1> <step1>) 
                    ////                           ...)
                    ////                           (<test> <expression> ...)
                    ////                         <command> ...)</r4rs>
                    return EvaluateDo.Call(this, this.args);

                case "time":
                    return EvaluateTime.Call(this, this.args);

                case "lambda":
                    // Evaluate a lambda by creating a closure.
                    //// <r4rs section="4.1.4">(lambda <formals> <body>)</r4rs>
                    //// <r4rs section="4.1.4">formals: (<variable1> ...)</r4rs>
                    //// <r4rs section="4.1.4">formals: <variable></r4rs>
                    //// <r4rs section="4.1.4">formals: (<variable 1> ... <variable n-1> . <variable n>)</r4rs>
                    return ReturnFromStep(new Closure(List.First(this.args), List.Rest(this.args), this.Env));

                case "macro":
                    // Evaluate a macro by creating a macro.
                    return ReturnFromStep(new Macro(List.First(this.args), List.Rest(this.args), this.Env));
            }

            // If we get here, it wasn't one of the special forms.  
            // So we need to evaluate the first item (the function) in preparation for
            //    doing a procedure call.
            //// <r4rs section="4.1.3">(<operator> <operand1> ...)</r4rs>
            this.Pc = this.ApplyProcStep;
            return Call(this, this.fn);
        }

        /// <summary>
        /// Come here after evaluating the first expression, the proc.
        /// Handle the proc: macro, closure, or function call.
        /// </summary>
        /// <returns>The next thing to do.</returns>
        private Stepper ApplyProcStep()
        {
            // Come here after evaluating fn
            // Make sure we don't assigne expr, since we need it intact below.
            this.fn = ReturnedExpr;
            this.Pc = this.ReturnStep;

            // If the function is a macro, expand it and then continue.
            if (this.fn is Macro)
            {
                return EvaluateExpandMacro.Call(this, this.args, (Macro)this.fn);
            }

            // If the function is a closure, then create a new environment consisting of
            //   1 the closure param list
            //   2 arguments evaluated in the original environment
            //   3 the closure's environment
            // Then continue evaluating the closure body in this new environment
            if (this.fn is Closure)
            {
                // CLOSURE CALL -- capture the environment and evaluate the body
                return EvaluateClosure.Call(this, this.args, (Closure)this.fn);
            }

            // This is a procedure call.
            // In any other case, the function is a primitive or a user-defined function.
            // Evaluate the arguments in the environment, then apply the function 
            //    to the arguments.
            return EvaluateApplyProc.Call(this, this.args, this.fn);
        }

        /// <summary>
        /// Here after an evaluation that should return a value.
        /// </summary>
        /// <returns>The evaluation result.</returns>
        private new Stepper ReturnStep()
        {
            // Assign return value and return to caller.
            return ReturnFromStep(this.ReturnedExpr, this.ReturnedEnv);
        }
    }
}