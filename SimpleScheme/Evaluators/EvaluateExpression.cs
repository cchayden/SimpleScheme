#define OLD
// <copyright file="EvaluateExpression.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using Obj = System.Object;

    /// <summary>
    /// The main evaluator for expressions.
    /// </summary>
   //// <r4rs section="4.1.3">(<operator> <operand1> ...)</r4rs>
    public sealed class EvaluateExpression : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        public const string StepperName = "evaluate-expression";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// The arguments to the function.
        /// </summary>
        private readonly Obj args;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateExpression class.
        /// The Expr is used to store the fn to evaluate.  
        /// The real expression being evaluated is Cons(fn, args).
        /// </summary>
        /// <param name="fn">The function to evaluate.</param>
        /// <param name="args">The function args to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateExpression(Obj fn, Obj args, Environment env, Stepper caller)
            : base(fn, env, caller)
        {
            this.args = args;

            ContinueHere(InitialStep);
            if (caller.Interp.Trace)
            {
                caller.Interp.CurrentOutputPort.WriteLine(String.Format("evaluate: {0} {1}", fn, args));
            }

            IncrementCounter(counter);
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the list primitives.
        /// Each of these could go through Call, but for those primitives that will really need to instantiate
        ///   EvaluateExpression, we just do that directly.
        /// Ususlly these constructs are not handled as primitives, but are instead
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(PrimitiveEnvironment env)
        {
            const int MaxInt = int.MaxValue;
            env
                //// <r4rs section="4.2.1">(and <test1> ...)</r4rs>
                .DefinePrimitive("and", (args, caller) => new EvaluateExpression("and", args, caller.Env, caller), 0, MaxInt)
                //// <r4rs section="4.2.3">(begin <expression1> <expression2> ...)</r4rs>
                //// <r4rs section="5.2">(begin <definition1> <definition2> ...)</r4rs>
                .DefinePrimitive("begin", (args, caller) => new EvaluateExpression("begin", args, caller.Env, caller), 0, MaxInt)
                .DefinePrimitive("parallel", (args, caller) => new EvaluateExpression("parallel", args, caller.Env, caller), 0, MaxInt)
                //// <r4rs section="4.2.1">(case <key> <clause1> <clause2> ...)<r4rs>
                //// <r4rs section="4.2.1">clause: ((<datum1> ...) <expression1> <expression2> ...)<r4rs>
                //// <r4rs section="4.2.1">else clause: (else <expression1> <expression2> ...)<r4rs>
                .DefinePrimitive("case", (args, caller) => new EvaluateExpression("case", args, caller.Env, caller), 0, MaxInt)
                //// <r4rs section="4.2.1">(cond <clause1> <clause2> ... ()</r4rs>
                //// <r4rs section="4.2.1">clause: (<test> <expression>)</r4rs>
                //// <r4rs section="4.2.1">clause: (<test> => <recipient>)</r4rs>
                //// <r4rs section="4.2.1">else clause: (else <expression1> <expression2> ...)</r4rs>
                .DefinePrimitive("cond", (args, caller) => new EvaluateExpression("cond", args, caller.Env, caller), 0, MaxInt)
                //// <r4rs section="5.2">(define <variable> <expression>)</r4rs>
                //// <r4rs section="5.2">(define (<variable> <formals>) <body>)</r4rs>
                //// <r4rs section="5.2">(define (<variable> . <formal>) <body>)</r4rs>
                .DefinePrimitive("define", (args, caller) => new EvaluateExpression("define", args, caller.Env, caller), 0, MaxInt)
                //// <r4rs section="4.2.4">(do ((variable1> <init1> <step1>) 
                ////                           ...)
                ////                           (<test> <expression> ...)
                ////                         <command> ...)</r4rs>
                .DefinePrimitive("do", (args, caller) => new EvaluateExpression("do", args, caller.Env, caller), 0, MaxInt)
                //// Instead of returning a value, return an evaulator that can be run to get the value
                .DefinePrimitive("eval", (args, caller) => Call(List.First(args), caller.Env, caller), 1, 2)
                //// <r4rs section="4.1.5">(if <test> <consequent> <alternate>)</r4rs>
                //// <r4rs section="4.1.5">(if <test> <consequent>)</r4rs>
                .DefinePrimitive("if", (args, caller) => new EvaluateExpression("if", args, caller.Env, caller), 0, MaxInt)
                //// <r4rs section="4.1.4">(lambda <formals> <body>)</r4rs>
                //// <r4rs section="4.1.4">formals: (<variable1> ...)</r4rs>
                //// <r4rs section="4.1.4">formals: <variable></r4rs>
                //// <r4rs section="4.1.4">formals: (<variable 1> ... <variable n-1> . <variable n>)</r4rs>
                .DefinePrimitive("lambda", (args, caller) => EvalLambda(args, caller.Env, caller), 0, MaxInt)
                //// <r4rs section="4.2.2">(let <bindings> <body>)</r4rs>
                //// <r4rs section="4.2.4">(let <variable> <bindings> <body>)</r4rs>
                //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
                //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
                .DefinePrimitive("let", (args, caller) => new EvaluateExpression("let", args, caller.Env, caller), 0, MaxInt)
                //// <r4rs section="4.2.2">(let* <bindings> <body>)</r4rs>
                //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
                //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
                .DefinePrimitive("let*", (args, caller) => new EvaluateExpression("let*", args, caller.Env, caller), 0, MaxInt)
                //// <r4rs section="4.2.2">(letrec <bindings> <body>)</r4rs>
                //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
                //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
                .DefinePrimitive("letrec", (args, caller) => new EvaluateExpression("letrec", args, caller.Env, caller), 0, MaxInt)
                .DefinePrimitive("macro", (args, caller) => EvalMacro(args, caller.Env, caller), 0, MaxInt)
                //// <r4rs section="4.2.1">(or <test1> ...)</r4rs>
                .DefinePrimitive("or", (args, caller) => new EvaluateExpression("or", args, caller.Env, caller), 0, MaxInt)
                //// <r4rs section="4.1.2">(quote <datum>)</r4rs>
                .DefinePrimitive("quote", (args, caller) => EvalQuote(args, caller), 0, MaxInt)
                //// <r4rs section="4.1.6">(set! <variable> <expression>)</r4rs>
                .DefinePrimitive("set!", (args, caller) => new EvaluateExpression("set!", args, caller.Env, caller), 0, MaxInt)
                //// (time <expr>)
                .DefinePrimitive("time", (args, caller) => new EvaluateExpression("time", args, caller.Env, caller), 0, MaxInt);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// First check for a form that does not need to create an evaluator.  If it is one of these,
        ///   just return the answer.  Otherwise create an EvaluateExpression to handle it.
        /// This version uses a supplied environment.  This is used with a closure
        ///   to evaluate in the closure's environment rather than in the current one.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to evaluate in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The evaluator.</returns>
        public static Stepper Call(Obj expr, Environment env, Stepper caller)
        {
            // If we don't need to do any steps, then
            // do not create an evaluator -- just return the value directly.
            //
            // First look for a symbol.
            if (Symbol.IsSymbol(expr))
            {
                // Evaluate a symbol by looking it up in the environment.
                // It should correspond to a variable name, for which there 
                //    is a corresponding value.
                if (env == null)
                {
                    return (Stepper)ErrorHandlers.SemanticError("EvaluateExpression: bad environment");
                }

                return caller.ContinueStep(env.UnsafeLookup(expr));
            }

            // Look for all other non-pair forms.
            if (!Pair.IsPair(expr))
            {
                // If we are evaluating something that is not a pair, 
                //    it must be a constant.
                // Return the integer, real, boolean, or vector.
                return caller.ContinueStep(expr);
            }

            // Break apart and evaluate the fn and args
            return Call(List.First(expr), List.Rest(expr), env, caller);
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Calls the main evaluator.
        /// Used by the primitives who have already separated the fun and args.
        /// Also used by the Call above after the args are separated.
        /// </summary>
        /// <param name="fn">The function to evaluate.</param>
        /// <param name="args">The args to evaluate.</param>
        /// <param name="env">The environment to evaluate in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The evaluator.</returns>
        private static Stepper Call(Obj fn, Obj args, Environment env, Stepper caller)
        {
            // Handle special forms that do not need an actual evaluuation.
            switch (fn as string)
            {
                case "quote":
                    // Evaluate quoted expression by just returning the expression.
                    //// <r4rs section="4.1.2">(quote <datum>)</r4rs>
                    return EvalQuote(args, caller);

                case "lambda":
                    // Evaluate a lambda by creating a closure.
                    //// <r4rs section="4.1.4">(lambda <formals> <body>)</r4rs>
                    //// <r4rs section="4.1.4">formals: (<variable1> ...)</r4rs>
                    //// <r4rs section="4.1.4">formals: <variable></r4rs>
                    //// <r4rs section="4.1.4">formals: (<variable 1> ... <variable n-1> . <variable n>)</r4rs>
                    return EvalLambda(args, env, caller);

                case "macro":
                    // Evaluate a macro by creating a macro.
                    return EvalMacro(args, env, caller);
            }

            // Actually create an evaluator to do the operation.
            return new EvaluateExpression(fn, args, env, caller);
        }

        /// <summary>
        /// Evaluate a quote expression.
        /// </summary>
        /// <param name="args">The args to quote.</param>
        /// <param name="caller">The caller.</param>
        /// <returns>The quoted expression.</returns>
        private static Stepper EvalQuote(Obj args, Stepper caller)
        {
            return caller.ContinueStep(List.First(args));
        }

        /// <summary>
        /// Evaluate a lambda expression
        /// </summary>
        /// <param name="args">The lambda args.</param>
        /// <param name="env">The execution environment.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The closure representing the lambda.</returns>
        private static Stepper EvalLambda(Obj args, Environment env, Stepper caller)
        {
            return caller.ContinueStep(new Closure(List.First(args), List.Rest(args), env));
        }

        /// <summary>
        /// Evaluate a macro.
        /// </summary>
        /// <param name="args">The macro args.</param>
        /// <param name="env">The execution environment.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The closure representing the lambda.</returns>
        private static Stepper EvalMacro(Obj args, Environment env, Stepper caller)
        {
            return caller.ContinueStep(new Macro(List.First(args), List.Rest(args), env));
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Start evaluation by testing the various forms.
        /// The special forms have their own evaluators.
        /// Otherwise, evaluate the first argument (the proc) in preparation for a call.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The next thing to do.</returns>
        private static Stepper InitialStep(Stepper s)
        {
            EvaluateExpression step = (EvaluateExpression)s;

            // defined as macro for now
            //// <r4rs section="4.2.6">(quasiquote <template>)</r4rs>
            //// <r4rs section="4.2.5">(delay <expression>)</r4rs>

            // Look for one of the special forms. 
            switch (s.Expr as string)
            {
                case "begin":
                    // Evaluate begin by evaluating all the items in order, 
                    //   and returning the last.
                    //// <r4rs section="4.2.3">(begin <expression1> <expression2> ...)</r4rs>
                    //// <r4rs section="5.2">(begin <definition1> <definition2> ...)</r4rs>
                    return EvaluateSequence.Call(step.args, s.Env, s.Caller);

                case "parallel":
                    // Evaluate begin by evaluating all the items in order.
                    // Return Undefined.
                    // If any suspend, keep going.
                    //// <r4rs section="4.2.3">(begin <expression1> <expression2> ...)</r4rs>
                    //// <r4rs section="5.2">(begin <definition1> <definition2> ...)</r4rs>
                    return EvaluateParallel.Call(step.args, s.Env, s.Caller);

                case "define":
                    // Define is a shortcut for lambda.
                    // Evaluate by splicing lambda on the front and evaluating that.
                    //// <r4rs section="5.2">(define <variable> <expression>)</r4rs>
                    //// <r4rs section="5.2">(define (<variable> <formals>) <body>)</r4rs>
                    //// <r4rs section="5.2">(define (<variable> . <formal>) <body>)</r4rs>
                    return EvaluateDefine.Call(step.args, s.Env, s.Caller);

                case "set!":
                    // Evaluate a set! expression by evaluating the second, 
                    //   then setting the first to it.
                    //// <r4rs section="4.1.6">(set! <variable> <expression>)</r4rs>
                    return EvaluateSet.Call(step.args, s.Env, s.ContinueHere(ReturnValueAndEnvStep));

                case "if":
                    // Eval an if expression by evaluating the first clause, 
                    //    and then returning either the second or third.
                    //// <r4rs section="4.1.5">(if <test> <consequent> <alternate>)</r4rs>
                    //// <r4rs section="4.1.5">(if <test> <consequent>)</r4rs>
                    return EvaluateIf.Call(step.args, s.Env, s.Caller);

                case "case":
                    //// <r4rs section="4.2.1">(case <key> <clause1> <clause2> ...)<r4rs>
                    //// <r4rs section="4.2.1">clause: ((<datum1> ...) <expression1> <expression2> ...)<r4rs>
                    //// <r4rs section="4.2.1">else clause: (else <expression1> <expression2> ...)<r4rs>
                    return EvaluateCase.Call(step.args, s.Env, s.Caller);

                case "or":
                    //// <r4rs section="4.2.1">(or <test1> ...)</r4rs>
                    return EvaluateOr.Call(step.args, s.Env, s.Caller);

                case "and":
                    //// <r4rs section="4.2.1">(and <test1> ...)</r4rs>
                    return EvaluateAnd.Call(step.args, s.Env, s.Caller);

                case "cond":
                    //// <r4rs section="4.2.1">(cond <clause1> <clause2> ... ()</r4rs>
                    //// <r4rs section="4.2.1">clause: (<test> <expression>)</r4rs>
                    //// <r4rs section="4.2.1">clause: (<test> => <recipient>)</r4rs>
                    //// <r4rs section="4.2.1">else clause: (else <expression1> <expression2> ...)</r4rs>
                    return EvaluateCond.Call(step.args, s.Env, s.Caller);

                case "let":
                    //// <r4rs section="4.2.2">(let <bindings> <body>)</r4rs>
                    //// <r4rs section="4.2.4">(let <variable> <bindings> <body>)</r4rs>
                    //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
                    //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
                    return EvaluateLet.Call(step.args, s.Env, s.Caller);

                case "let*":
                    //// <r4rs section="4.2.2">(let* <bindings> <body>)</r4rs>
                    //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
                    //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
                    return EvaluateLetStar.Call(step.args, s.Env, s.Caller);

                case "letrec":
                    //// <r4rs section="4.2.2">(letrec <bindings> <body>)</r4rs>
                    //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
                    //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
                    return EvaluateLetRec.Call(step.args, s.Env, s.Caller);

                case "do":
                    //// <r4rs section="4.2.4">(do ((variable1> <init1> <step1>) 
                    ////                           ...)
                    ////                           (<test> <expression> ...)
                    ////                         <command> ...)</r4rs>
                    return EvaluateDo.Call(step.args, s.Env, s.Caller);

                case "time":
                    return EvaluateTime.Call(step.args, s.Env, s.Caller);
            }

            // If we get here, it wasn't one of the special forms.  
            // So we need to evaluate the first item (the function) in preparation for
            //    doing a procedure call.
            //// <r4rs section="4.1.3">(<operator> <operand1> ...)</r4rs>
            return Call(s.Expr, s.Env, s.ContinueHere(ApplyProcStep));
        }

        /// <summary>
        /// Come here after evaluating the first expression, the proc.
        /// Handle the proc: macro, closure, or function call.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The next thing to do.</returns>
        private static Stepper ApplyProcStep(Stepper s)
        {
            EvaluateExpression step = (EvaluateExpression)s;

            // Come here after evaluating fn
            return Procedure.AsProcedure(s.ReturnedExpr).Evaluate(step.args, s.Env, s.Caller);
        }

        /// <summary>
        /// Here after an evaluation that should return a value.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The evaluation result.</returns>
        private static Stepper ReturnValueAndEnvStep(Stepper s)
        {
            // Assign return value and return to caller.
            return s.ReturnFromStep(s.ReturnedExpr, s.ReturnedEnv);
        }
        #endregion
    }
}