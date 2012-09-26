// <copyright file="EvaluateExpression.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// The main evaluator for expressions.
    /// </summary>
   //// <r4rs section="4.1.3">(<operator> <operand1> ...)</r4rs>
    public sealed class EvaluateExpression : Evaluator
    {
        #region Constants
        private static readonly Symbol andSym = Symbol.New("and");
        private static readonly Symbol beginSym = Symbol.New("begin");
        private static readonly Symbol parallelSym = Symbol.New("parallel");
        private static readonly Symbol caseSym = Symbol.New("case");
        private static readonly Symbol condSym = Symbol.New("cond");
        private static readonly Symbol defineSym = Symbol.New("define");
        private static readonly Symbol doSym = Symbol.New("do");
        private static readonly Symbol ifSym = Symbol.New("if");
        private static readonly Symbol letSym = Symbol.New("let");
        private static readonly Symbol letstarSym = Symbol.New("let*");
        private static readonly Symbol letrecSym = Symbol.New("letrec");
        private static readonly Symbol orSym = Symbol.New("or");
        private static readonly Symbol setSym = Symbol.New("set!");
        private static readonly Symbol timeSym = Symbol.New("time");

        #endregion

        #region Fields
        /// <summary>
        /// The name of the evaluator, used for counters and tracing.
        /// </summary>
        public const string EvaluatorName = "evaluate-expression";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(EvaluatorName);

        /// <summary>
        /// The function to evaluate.
        /// </summary>
        private readonly Obj fn;
        #endregion

        #region Constructor

        /// <summary>
        /// Initializes a new instance of the EvaluateExpression class.
        /// The Expr is used to store the fn to evaluate.  
        /// The real expression being evaluated is Cons(fn, args).
        /// </summary>
        /// <param name="args">The function args to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="fn">The function to evaluate.</param>
        private EvaluateExpression(Obj args, Environment env, Evaluator caller, Obj fn)
            : base(args, env, caller)
        {
            this.fn = fn;

            ContinueHere(InitialStep);
            if (caller.Interp.Trace)
            {
                caller.Interp.CurrentOutputPort.WriteLine(string.Format("evaluate: {0} {1}", fn, args));
            }

            IncrementCounter(counter);
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the list primitives.
        /// Each of these could go through Apply, but for those primitives that will really need to instantiate
        ///   EvaluateExpression, we just do that directly.
        /// Ususlly these constructs are not handled as primitives, but are instead
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(PrimitiveEnvironment env)
        {
            const int MaxInt = int.MaxValue;
            env
                //// <r4rs section="4.2.1">(and <test1> ...)</r4rs>
                .DefinePrimitive(
                        andSym,
                        (args, caller) => new EvaluateExpression(args, caller.Env, caller, andSym), 
                        0, 
                        MaxInt, 
                        Primitive.ValueType.Obj)
                //// <r4rs section="4.2.3">(begin <expression1> <expression2> ...)</r4rs>
                //// <r4rs section="5.2">(begin <definition1> <definition2> ...)</r4rs>
                .DefinePrimitive(
                        beginSym, 
                        (args, caller) => new EvaluateExpression(args, caller.Env, caller, beginSym), 
                        0, 
                        MaxInt, 
                        Primitive.ValueType.Obj)
                //// (parallel <expr> ...)
                .DefinePrimitive(
                        parallelSym, 
                        (args, caller) => new EvaluateExpression(args, caller.Env, caller, parallelSym), 
                        0, 
                        MaxInt, 
                        Primitive.ValueType.Pair)
                //// <r4rs section="4.2.1">(case <key> <clause1> <clause2> ...)<r4rs>
                //// <r4rs section="4.2.1">clause: ((<datum1> ...) <expression1> <expression2> ...)<r4rs>
                //// <r4rs section="4.2.1">else clause: (else <expression1> <expression2> ...)<r4rs>
                .DefinePrimitive(
                        caseSym, 
                        (args, caller) => new EvaluateExpression(args, caller.Env, caller, caseSym), 
                        0, 
                        MaxInt, 
                        Primitive.ValueType.Pair)
                //// <r4rs section="4.2.1">(cond <clause1> <clause2> ... )</r4rs>
                //// <r4rs section="4.2.1">clause: (<test> <expression>)</r4rs>
                //// <r4rs section="4.2.1">clause: (<test> => <recipient>)</r4rs>
                //// <r4rs section="4.2.1">else clause: (else <expression1> <expression2> ...)</r4rs>
                .DefinePrimitive(
                        condSym, 
                        (args, caller) => new EvaluateExpression(args, caller.Env, caller, condSym), 
                        0, 
                        MaxInt, 
                        Primitive.ValueType.Obj)
                //// <r4rs section="5.2">(define <variable> <expression>)</r4rs>
                //// <r4rs section="5.2">(define (<variable> <formals>) <body>)</r4rs>
                //// <r4rs section="5.2">(define (<variable> . <formal>) <body>)</r4rs>
                .DefinePrimitive(
                        defineSym, 
                        (args, caller) => new EvaluateExpression(args, caller.Env, caller, defineSym), 
                        0, 
                        MaxInt, 
                        Primitive.ValueType.PairOrSymbol)
                //// <r4rs section="4.2.4">(do ((variable1> <init1> <step1>) 
                ////                           ...)
                ////                           (<test> <expression> ...)
                ////                         <command> ...)</r4rs>
                .DefinePrimitive(
                        doSym, 
                        (args, caller) => new EvaluateExpression(args, caller.Env, caller, doSym), 
                        0, 
                        MaxInt, 
                        Primitive.ValueType.Pair)
                //// Instead of returning a value, return an evaulator that can be run to get the value
                .DefinePrimitive(
                        Symbol.New("eval"), 
                        (args, caller) => Call(args.First(), caller.Env, caller), 
                        1, 
                        2, 
                        Primitive.ValueType.Obj)
                //// <r4rs section="4.1.5">(if <test> <consequent> <alternate>)</r4rs>
                //// <r4rs section="4.1.5">(if <test> <consequent>)</r4rs>
                .DefinePrimitive(
                        ifSym, 
                        (args, caller) => new EvaluateExpression(args, caller.Env, caller, ifSym), 
                        0, 
                        MaxInt, 
                        Primitive.ValueType.Obj)
                //// <r4rs section="4.1.4">(lambda <formals> <body>)</r4rs>
                //// <r4rs section="4.1.4">formals: (<variable1> ...)</r4rs>
                //// <r4rs section="4.1.4">formals: <variable></r4rs>
                //// <r4rs section="4.1.4">formals: (<variable 1> ... <variable n-1> . <variable n>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("lambda"), 
                        (args, caller) => EvalLambda(args, caller.Env, caller), 
                        0, 
                        MaxInt, 
                        Primitive.ValueType.PairOrSymbol)
                //// <r4rs section="4.2.2">(let <bindings> <body>)</r4rs>
                //// <r4rs section="4.2.4">(let <variable> <bindings> <body>)</r4rs>
                //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
                //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
                .DefinePrimitive(
                        letSym, 
                        (args, caller) => new EvaluateExpression(args, caller.Env, caller, letSym), 
                        0, 
                        MaxInt, 
                        Primitive.ValueType.PairOrSymbol)
                //// <r4rs section="4.2.2">(let* <bindings> <body>)</r4rs>
                //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
                //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
                .DefinePrimitive(
                        letstarSym, 
                        (args, caller) => new EvaluateExpression(args, caller.Env, caller, letstarSym), 
                        0, 
                        MaxInt, 
                        Primitive.ValueType.Pair)
                //// <r4rs section="4.2.2">(letrec <bindings> <body>)</r4rs>
                //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
                //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
                .DefinePrimitive(
                        letrecSym, 
                        (args, caller) => new EvaluateExpression(args, caller.Env, caller, letrecSym), 
                        0, 
                        MaxInt, 
                        Primitive.ValueType.Pair)
                //// not defined in r4rs
                .DefinePrimitive(
                        Symbol.New("macro"), 
                        (args, caller) => EvalMacro(args, caller.Env, caller), 
                        0, 
                        MaxInt, 
                        Primitive.ValueType.Pair)
                //// <r4rs section="4.2.1">(or <test1> ...)</r4rs>
                .DefinePrimitive(
                        orSym, 
                        (args, caller) => new EvaluateExpression(args, caller.Env, caller, orSym), 
                        0, 
                        MaxInt, 
                        Primitive.ValueType.Obj)
                //// <r4rs section="4.1.2">(quote <datum>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("quote"), 
                        (args, caller) => EvalQuote(args, caller), 
                        1, 
                        Primitive.ValueType.Obj)
                //// <r4rs section="4.1.6">(set! <variable> <expression>)</r4rs>
                .DefinePrimitive(
                    setSym,
                    (args, caller) => new EvaluateExpression(args, caller.Env, caller, setSym),
                    2,
                    Primitive.ValueType.Symbol,
                    Primitive.ValueType.Pair)
                //// (time <expr>)
                .DefinePrimitive(
                        timeSym, 
                        (args, caller) => new EvaluateExpression(args, caller.Env, caller, timeSym), 
                        1, 
                        Primitive.ValueType.Obj);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// First check for a form that does not need to create an evaluator.  If it is one of these,
        ///   just return the answer.  Otherwise create an EvaluateExpression to handle it.
        /// This version uses a supplied environment.  This is used with a lambda
        ///   to evaluate in the lambda's environment rather than in the current one.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to evaluate in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The evaluator.</returns>
        public static Evaluator Call(Obj expr, Environment env, Evaluator caller)
        {
            // If we don't need to do any steps, then
            // do not create an evaluator -- just return the value directly.
            //
            // First look for a symbol.
            if (expr.IsSymbol())
            {
                // Evaluate a symbol by looking it up in the environment.
                // It should correspond to a variable name, for which there 
                //    is a corresponding value.
                if (env == null)
                {
                    return (Evaluator)ErrorHandlers.SemanticError("EvaluateExpression: bad environment");
                }

                return caller.UpdateReturnValue(env.Lookup(expr.AsSymbol()));
            }

            // Look for all other non-pair forms.
            if (!expr.IsPair())
            {
                // If we are evaluating something that is not a pair, 
                //    it must be a constant.
                // Return the integer, real, boolean, or vector.
                return caller.UpdateReturnValue(expr);
            }

            // Break apart and evaluate the fn and args
            // Handle special forms that do not need an actual evaluation.
            Obj fn = expr.First();
            Obj args = expr.Rest();
            if (fn.IsSymbol())
            {
                switch (fn.ToString())
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
            }

            // Actually create an evaluator to do the operation.
            return new EvaluateExpression(args, env, caller, fn);
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Evaluate a quote expression.
        /// </summary>
        /// <param name="args">The args to quote.</param>
        /// <param name="caller">The caller.</param>
        /// <returns>The quoted expression.</returns>
        private static Evaluator EvalQuote(Obj args, Evaluator caller)
        {
            return caller.UpdateReturnValue(args.First());
        }

        /// <summary>
        /// Evaluate a lambda expression
        /// </summary>
        /// <param name="args">The lambda args.</param>
        /// <param name="env">The execution environment.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The lambda representing the expression.</returns>
        private static Evaluator EvalLambda(Obj args, Environment env, Evaluator caller)
        {
            return caller.UpdateReturnValue(Lambda.New(args.First(), args.Rest(), env));
        }

        /// <summary>
        /// Evaluate a macro.
        /// </summary>
        /// <param name="args">The macro args.</param>
        /// <param name="env">The execution environment.</param>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>The macro representing the expression.</returns>
        private static Evaluator EvalMacro(Obj args, Environment env, Evaluator caller)
        {
            return caller.UpdateReturnValue(Macro.New(args.First(), args.Rest(), env));
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Start evaluation by testing the various forms.
        /// The special forms have their own evaluators.
        /// Otherwise, evaluate the first argument (the proc) in preparation for a call.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next thing to do.</returns>
        private static Evaluator InitialStep(Evaluator s)
        {
            var step = (EvaluateExpression)s;

            // defined as macro for now
            //// <r4rs section="4.2.6">(quasiquote <template>)</r4rs>
            //// <r4rs section="4.2.5">(delay <expression>)</r4rs>

            // Look for one of the special forms. 
            if (step.fn.IsSymbol())
            {
                switch (step.fn.ToString())
                {
                    case "begin":
                        // Evaluate begin by evaluating all the items in order, 
                        //   and returning the last.
                        //// <r4rs section="4.2.3">(begin <expression1> <expression2> ...)</r4rs>
                        //// <r4rs section="5.2">(begin <definition1> <definition2> ...)</r4rs>
                        return EvaluateSequence.Call(s.Expr, s.Env, s.Caller);

                    case "parallel":
                        // Evaluate begin by evaluating all the items in order.
                        // Return Undefined.
                        // If any suspend, keep going.
                        //// <r4rs section="4.2.3">(begin <expression1> <expression2> ...)</r4rs>
                        //// <r4rs section="5.2">(begin <definition1> <definition2> ...)</r4rs>
                        return EvaluateParallel.Call(s.Expr, s.Env, s.Caller);

                    case "define":
                        // Define is a shortcut for lambda.
                        // Evaluate by splicing lambda on the front and evaluating that.
                        //// <r4rs section="5.2">(define <variable> <expression>)</r4rs>
                        //// <r4rs section="5.2">(define (<variable> <formals>) <body>)</r4rs>
                        //// <r4rs section="5.2">(define (<variable> . <formal>) <body>)</r4rs>
                        return EvaluateDefine.Call(s.Expr, s.Env, s.Caller);

                    case "set!":
                        // Evaluate a set! expression by evaluating the second, 
                        //   then setting the first to it.
                        //// <r4rs section="4.1.6">(set! <variable> <expression>)</r4rs>
                        return EvaluateSet.Call(s.Expr, s.Env, s.Caller);

                    case "increment!":
                        //// <r4rs section="none">(increment! <variable>)</r4rs>
                        return Increment(s.Expr, s.Env, s.Caller);

                    case "if":
                        // Eval an if expression by evaluating the first clause, 
                        //    and then returning either the second or third.
                        //// <r4rs section="4.1.5">(if <test> <consequent> <alternate>)</r4rs>
                        //// <r4rs section="4.1.5">(if <test> <consequent>)</r4rs>
                        return EvaluateIf.Call(s.Expr, s.Env, s.Caller);

                    case "case":
                        //// <r4rs section="4.2.1">(case <key> <clause1> <clause2> ...)<r4rs>
                        //// <r4rs section="4.2.1">clause: ((<datum1> ...) <expression1> <expression2> ...)<r4rs>
                        //// <r4rs section="4.2.1">else clause: (else <expression1> <expression2> ...)<r4rs>
                        return EvaluateCase.Call(s.Expr, s.Env, s.Caller);

                    case "or":
                        //// <r4rs section="4.2.1">(or <test1> ...)</r4rs>
                        return EvaluateOr.Call(s.Expr, s.Env, s.Caller);

                    case "and":
                        //// <r4rs section="4.2.1">(and <test1> ...)</r4rs>
                        return EvaluateAnd.Call(s.Expr, s.Env, s.Caller);

                    case "cond":
                        //// <r4rs section="4.2.1">(cond <clause1> <clause2> ... ()</r4rs>
                        //// <r4rs section="4.2.1">clause: (<test> <expression>)</r4rs>
                        //// <r4rs section="4.2.1">clause: (<test> => <recipient>)</r4rs>
                        //// <r4rs section="4.2.1">else clause: (else <expression1> <expression2> ...)</r4rs>
                        return EvaluateCond.Call(s.Expr, s.Env, s.Caller);

                    case "let":
                        //// <r4rs section="4.2.2">(let <bindings> <body>)</r4rs>
                        //// <r4rs section="4.2.4">(let <variable> <bindings> <body>)</r4rs>
                        //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
                        //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
                        return EvaluateLet.Call(s.Expr, s.Env, s.Caller);

                    case "let*":
                        //// <r4rs section="4.2.2">(let* <bindings> <body>)</r4rs>
                        //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
                        //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
                        return EvaluateLetStar.Call(s.Expr, s.Env, s.Caller);

                    case "letrec":
                        //// <r4rs section="4.2.2">(letrec <bindings> <body>)</r4rs>
                        //// <r4rs section="4.2.4">bindings: ((<variable1> <init1>) ...)</r4rs>
                        //// <r4rs section="4.2.4">body: <expression> ...</r4rs>
                        return EvaluateLetRec.Call(s.Expr, s.Env, s.Caller);

                    case "do":
                        //// <r4rs section="4.2.4">(do ((variable1> <init1> <step1>) 
                        ////                           ...)
                        ////                           (<test> <expression> ...)
                        ////                         <command> ...)</r4rs>
                        return EvaluateDo.Call(s.Expr, s.Env, s.Caller);

                    case "time":
                        //// (time <expr>)
                        return EvaluateTime.Call(s.Expr, s.Env, s.Caller);
                }
            }

            // If we get here, it wasn't one of the special forms.  
            // So we need to evaluate the first item (the function) in preparation for
            //    doing a procedure call.
            //// <r4rs section="4.1.3">(<operator> <operand1> ...)</r4rs>
            return Call(step.fn, s.Env, s.ContinueHere(ApplyProcStep));
        }

        /// <summary>
        /// Increment the variable.
        /// The intention is that this should be atomic.
        /// </summary>
        /// <param name="expr">The symbol whose value is incremented.</param>
        /// <param name="env">The environment.</param>
        /// <param name="caller">Return to this caller.</param>
        /// <returns>The next step to execute.</returns>
        private static Evaluator Increment(Obj expr, Environment env, Evaluator caller)
        {
            Obj lhs = expr.First();
            if (!lhs.IsSymbol())
            {
                ErrorHandlers.SemanticError("Increment: first argument must be a symbol.  Got: " + lhs);
            }

            return caller.UpdateReturnValue(env.Increment(lhs));
        }

        /// <summary>
        /// Come here after evaluating the first expression, the proc.
        /// Handle the proc: macro, lambda, or function call.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next thing to do.</returns>
        private static Evaluator ApplyProcStep(Evaluator s)
        {
            // Come here after evaluating fn
            if (!s.ReturnedExpr.IsProcedure())
            {
                ErrorHandlers.SemanticError("Value must be procedure: " + s.ReturnedExpr);
            }

            return s.ReturnedExpr.AsProcedure().Evaluate(s.Expr, s.Env, s.Caller);
        }
        #endregion
    }
}