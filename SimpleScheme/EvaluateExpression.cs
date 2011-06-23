// <copyright file="EvaluateExpression.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// The main evaluator for expressions.
    /// </summary>
   //// <r4rs section="4.1.3">(<operator> <operand1> ...)</r4rs>
    public sealed class EvaluateExpression : Stepper
    {
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "evaluate-expression";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

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
        /// Initializes a new instance of the EvaluateExpression class.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateExpression(Stepper caller, object expr, Environment env)
            : base(caller, expr, env)
        {
            // We are evaluating a pair.
            // Split out the first item for special treatment.
            this.fn = First(Expr);
            this.args = Rest(Expr);

            ContinueHere(this.InitialStep);
            this.trace = false;
            IncrementCounter(counter);
        }

        /// <summary>
        /// Gets the name of the stepper.
        /// </summary>
        public override string Name
        {
            get { return StepperName; }
        }

        /// <summary>
        /// Define the list primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(Environment env)
        {
            const int MaxInt = int.MaxValue;
            env
                //// <r4rs section="4.2.1">(and <test1> ...)</r4rs>
                .DefinePrimitive("and", (caller, args) => new EvaluateExpression(caller, Cons("and", args), caller.Env), 0, MaxInt)
                //// <r4rs section="4.2.3">(begin <expression1> <expression2> ...)</r4rs>
                //// <r4rs section="5.2">(begin <definition1> <definition2> ...)</r4rs>
                .DefinePrimitive("begin", (caller, args) => new EvaluateExpression(caller, Cons("begin", args), caller.Env), 0, MaxInt)
                //// <r4rs section="4.2.1">(case <key> <clause1> <clause2> ...)<r4rs>
                //// <r4rs section="4.2.1">clause: ((<datum1> ...) <expression1> <expression2> ...)<r4rs>
                //// <r4rs section="4.2.1">else clause: (else <expression1> <expression2> ...)<r4rs>
                .DefinePrimitive("case", (caller, args) => new EvaluateExpression(caller, Cons("case", args), caller.Env), 0, MaxInt)
                //// <r4rs section="4.2.1">(cond <clause1> <clause2> ... ()</r4rs>
                //// <r4rs section="4.2.1">clause: (<test> <expression>)</r4rs>
                //// <r4rs section="4.2.1">clause: (<test> => <recipient>)</r4rs>
                //// <r4rs section="4.2.1">else clause: (else <expression1> <expression2> ...)</r4rs>
                .DefinePrimitive("cond", (caller, args) => new EvaluateExpression(caller, Cons("cond", args), caller.Env), 0, MaxInt)
                //// <r4rs section="5.2">(define <variable> <expression>)</r4rs>
                //// <r4rs section="5.2">(define (<variable> <formals>) <body>)</r4rs>
                //// <r4rs section="5.2">(define (<variable> . <formal>) <body>)</r4rs>
                .DefinePrimitive("define", (caller, args) => new EvaluateExpression(caller, Cons("define", args), caller.Env), 0, MaxInt)
                //// <r4rs section="4.2.4">(do ((variable1> <init1> <step1>) 
                ////                           ...)
                ////                           (<test> <expression> ...)
                ////                         <command> ...)</r4rs>
                .DefinePrimitive("do", (caller, args) => new EvaluateExpression(caller, Cons("do", args), caller.Env), 0, MaxInt)
                //// <r4rs section="4.1.5">(if <test> <consequent> <alternate>)</r4rs>
                //// <r4rs section="4.1.5">(if <test> <consequent>)</r4rs>
                .DefinePrimitive("if", (caller, args) => new EvaluateExpression(caller, Cons("if", args), caller.Env), 0, MaxInt)
                //// <r4rs section="4.1.4">(lambda <formals> <body>)</r4rs>
                //// <r4rs section="4.1.4">formals: (<variable1> ...)</r4rs>
                //// <r4rs section="4.1.4">formals: <variable></r4rs>
                //// <r4rs section="4.1.4">formals: (<variable 1> ... <variable n-1> . <variable n>)</r4rs>
                .DefinePrimitive("lambda", (caller, args) => Call(caller, Cons("lambda", args), caller.Env), 0, MaxInt)
                    //// <r4rs section="4.2.2">(let <bindings> <body>)</r4rs>
                    //// <r4rs section="4.2.4">(let <variable> <bindings> <body>)</r4rs>
                    //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
                    //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
                .DefinePrimitive("let", (caller, args) => new EvaluateExpression(caller, Cons("let", args), caller.Env), 0, MaxInt)
                //// <r4rs section="4.2.2">(let* <bindings> <body>)</r4rs>
                //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
                //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
                .DefinePrimitive("let*", (caller, args) => new EvaluateExpression(caller, Cons("let*", args), caller.Env), 0, MaxInt)
                //// <r4rs section="4.2.2">(letrec <bindings> <body>)</r4rs>
                //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
                //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
                .DefinePrimitive("letrec", (caller, args) => new EvaluateExpression(caller, Cons("letrec", args), caller.Env), 0, MaxInt)
                .DefinePrimitive("macro", (caller, args) => Call(caller, Cons("macro", args), caller.Env), 0, MaxInt)
                //// <r4rs section="4.2.1">(or <test1> ...)</r4rs>
                .DefinePrimitive("or", (caller, args) => new EvaluateExpression(caller, Cons("or", args), caller.Env), 0, MaxInt)
                //// <r4rs section="4.1.2">(quote <datum>)</r4rs>
                .DefinePrimitive("quote", (caller, args) => Call(caller, Cons("quote", args), caller.Env), 0, MaxInt)
                //// <r4rs section="4.1.6">(set! <variable> <expression>)</r4rs>
                .DefinePrimitive("set!", (caller, args) => new EvaluateExpression(caller, Cons("set!", args), caller.Env), 0, MaxInt)
                //// time
                .DefinePrimitive("time", (caller, args) => new EvaluateExpression(caller, Cons("time", args), caller.Env), 0, MaxInt);
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
        public static Stepper Call(Stepper caller, object expr, Environment env)
        {
            // If we don't need to do any steps, then
            // do not create an evaluator object -- just return the value directly.
            if (expr is string)
            {
                // Evaluate a string by looking it up in the environment.
                // It should correspond to a varialbe name, for which there 
                //    is a corresponding value.
                return caller.ContinueStep(env.Lookup((string)expr));
            }

            if (!(expr is Pair))
            {
                // If we are evaluating something that is not a pair, 
                //    it must be a constant.
                // Return the integer, real, boolean, or vector.
                return caller.ContinueStep(expr);
            }

            object fn = First(expr);
            object args = Rest(expr);

            switch (fn as string)
            {
                case "quote":
                    // Evaluate quoted expression by just returning the expression.
                    //// <r4rs section="4.1.2">(quote <datum>)</r4rs>
                    return caller.ContinueStep(First(args));

                case "lambda":
                    // Evaluate a lambda by creating a closure.
                    //// <r4rs section="4.1.4">(lambda <formals> <body>)</r4rs>
                    //// <r4rs section="4.1.4">formals: (<variable1> ...)</r4rs>
                    //// <r4rs section="4.1.4">formals: <variable></r4rs>
                    //// <r4rs section="4.1.4">formals: (<variable 1> ... <variable n-1> . <variable n>)</r4rs>
                    return caller.ContinueStep(new Closure(First(args), Rest(args), env));

                case "macro":
                    // Evaluate a macro by creating a macro.
                    return caller.ContinueStep(new Macro(First(args), Rest(args), env));
            }

            // Actually create an evaluator to do the operation.
            return new EvaluateExpression(caller, expr, env);
        }

        /// <summary>
        /// Calls the main evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
        {
            return Call(caller, expr, caller.Env);
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
            if (this.trace)
            {
                Console.Out.WriteLine("{0} {1}", this.fn, this.args);
            }

            // defined as macro for now
            //// <r4rs section="4.2.6">(quasiquote <template>)</r4rs>
            //// <r4rs section="4.2.5">(delay <expression>)</r4rs>

            // Look for one of the special forms. 
            switch (this.fn as string)
            {
                case "begin":
                    // Evaluate begin by evaluating all the items in order, 
                    //   and returning the last.
                    //// <r4rs section="4.2.3">(begin <expression1> <expression2> ...)</r4rs>
                    //// <r4rs section="5.2">(begin <definition1> <definition2> ...)</r4rs>
                    return EvaluateSequence.Call(ContinueReturn(), this.args);

                case "define":
                    // Define is a shortcut for lambda.
                    // Evaluate by splicing lambda on the front and evaluating that.
                    //// <r4rs section="5.2">(define <variable> <expression>)</r4rs>
                    //// <r4rs section="5.2">(define (<variable> <formals>) <body>)</r4rs>
                    //// <r4rs section="5.2">(define (<variable> . <formal>) <body>)</r4rs>
                    return EvaluateDefine.Call(ContinueReturn(), this.args);

                case "set!":
                    // Evaluate a set! expression by evaluating the second, 
                    //   then setting the first to it.
                    //// <r4rs section="4.1.6">(set! <variable> <expression>)</r4rs>
                    return EvaluateSet.Call(ContinueHere(this.ReturnStep), this.args);

                case "if":
                    // Eval an if expression by evaluating the first clause, 
                    //    and then returning either the second or third.
                    //// <r4rs section="4.1.5">(if <test> <consequent> <alternate>)</r4rs>
                    //// <r4rs section="4.1.5">(if <test> <consequent>)</r4rs>
                    return EvaluateIf.Call(ContinueReturn(), this.args);

                case "case":
                    //// <r4rs section="4.2.1">(case <key> <clause1> <clause2> ...)<r4rs>
                    //// <r4rs section="4.2.1">clause: ((<datum1> ...) <expression1> <expression2> ...)<r4rs>
                    //// <r4rs section="4.2.1">else clause: (else <expression1> <expression2> ...)<r4rs>
                    return EvaluateCase.Call(ContinueReturn(), this.args);

                case "or":
                    //// <r4rs section="4.2.1">(or <test1> ...)</r4rs>
                    return EvaluateOr.Call(ContinueReturn(), this.args);

                case "and":
                    //// <r4rs section="4.2.1">(and <test1> ...)</r4rs>
                    return EvaluateAnd.Call(ContinueReturn(), this.args);

                case "cond":
                    //// <r4rs section="4.2.1">(cond <clause1> <clause2> ... ()</r4rs>
                    //// <r4rs section="4.2.1">clause: (<test> <expression>)</r4rs>
                    //// <r4rs section="4.2.1">clause: (<test> => <recipient>)</r4rs>
                    //// <r4rs section="4.2.1">else clause: (else <expression1> <expression2> ...)</r4rs>
                    return EvaluateCond.Call(ContinueReturn(), this.args);

                case "let":
                    //// <r4rs section="4.2.2">(let <bindings> <body>)</r4rs>
                    //// <r4rs section="4.2.4">(let <variable> <bindings> <body>)</r4rs>
                    //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
                    //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
                    return EvaluateLet.Call(ContinueReturn(), this.args);

                case "let*":
                    //// <r4rs section="4.2.2">(let* <bindings> <body>)</r4rs>
                    //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
                    //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
                    return EvaluateLetStar.Call(ContinueReturn(), this.args);

                case "letrec":
                    //// <r4rs section="4.2.2">(letrec <bindings> <body>)</r4rs>
                    //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
                    //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
                    return EvaluateLetRec.Call(ContinueReturn(), this.args);

                case "do":
                    //// <r4rs section="4.2.4">(do ((variable1> <init1> <step1>) 
                    ////                           ...)
                    ////                           (<test> <expression> ...)
                    ////                         <command> ...)</r4rs>
                    return EvaluateDo.Call(ContinueReturn(), this.args);

                case "time":
                    return EvaluateTime.Call(ContinueReturn(), this.args);
            }

            // If we get here, it wasn't one of the special forms.  
            // So we need to evaluate the first item (the function) in preparation for
            //    doing a procedure call.
            //// <r4rs section="4.1.3">(<operator> <operand1> ...)</r4rs>
            return Call(ContinueHere(this.ApplyProcStep), this.fn);
        }

        /// <summary>
        /// Come here after evaluating the first expression, the proc.
        /// Handle the proc: macro, closure, or function call.
        /// </summary>
        /// <returns>The next thing to do.</returns>
        private Stepper ApplyProcStep()
        {
            // Come here after evaluating fn
            return Procedure.Proc(ReturnedExpr).Evaluate(ContinueReturn(), this.args);
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