#define OLDx
// <copyright file="EvaluateExpression.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;

    /// <summary>
    /// The main evaluator for expressions.
    /// </summary>
   //// <r4rs section="4.1.3">(<operator> <operand1> ...)</r4rs>
    public sealed class EvaluateExpression : Evaluator
    {
        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-expression");

        /// <summary>
        /// Maps symbol names to special actions.
        /// </summary>
        private static readonly Dictionary<string, SpecialAction> specialActions;

        /// <summary>
        /// The function to evaluate.
        /// </summary>
        private readonly SchemeObject fn;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes static members of the <see cref="EvaluateExpression"/> class.
        /// </summary>
        static EvaluateExpression()
        {
            specialActions = new Dictionary<string, SpecialAction>
                {
                    { "begin", EvaluateSequence.Call },
                    { "parallel", EvaluateParallel.Call },
                    { "define", EvaluateDefine.Call },
                    { "set!", EvaluateSet.Call },
                    { "increment!", Increment },
                    { "if", EvaluateIf.Call },
                    { "case", EvaluateCase.Call },
                    { "or", EvaluateOr.Call },
                    { "and", EvaluateAnd.Call },
                    { "cond", EvaluateCond.Call },
                    { "let", EvaluateLet.Call },
                    { "let*", EvaluateLetStar.Call },
                    { "letrec", EvaluateLetRec.Call },
                    { "do", EvaluateDo.Call },
                    { "time", EvaluateTime.Call },
                    { "quote", EvalQuote },
                    { "lambda", Lambda.Call },
                    { "macro", Macro.Call }
                };
        }

        /// <summary>
        /// Initializes a new instance of the EvaluateExpression class.
        /// At this point, fn is something that evaluates to a proc.
        /// It might be a symbol that, when looked up, resolves to a primitive or to a
        ///   proc of some kind, like a lambda.
        /// It is not a symbol like "and" where there are special argument-evaluation rules, so
        ///   the first task is to evaluate all the arguments.
        /// </summary>
        /// <param name="args">The function args to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="fn">The function to evaluate.</param>
        private EvaluateExpression(SchemeObject args, Environment env, Evaluator caller, SchemeObject fn)
            : base(args, env, caller, counter)
        {
            this.fn = fn;

            this.ContinueAt(InitialStep);
#if Diagnostics
            if (caller.Interp.Trace)
            {
                caller.Interp.CurrentOutputPort.WriteLine(string.Format("evaluate: {0} applied to {1}", fn, args));
            }
#endif
        }
        #endregion

        #region Delegates
        /// <summary>
        /// The signature of all apecial actions.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment.</param>
        /// <param name="caller">The caller, used to return to.</param>
        /// <returns>The evaluator to execute next.</returns>
        private delegate Evaluator SpecialAction(SchemeObject expr, Environment env, Evaluator caller);
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the list primitives.
        /// Each of these could go through Apply, but for those primitives that will really need to instantiate
        ///   EvaluateExpression, we just do that directly.
        /// Usually we can recognize these in InitialStep of the caller and dispatch the correct Evaluator without having to
        ///   instantiate another evaluator.  But if the primitive is assigned to a variable and the variable appears in the
        ///   proc spot, it gets here.  For instance ((lambda (fun)(fun 0)) and) will use this primitive, whereas
        ///   (and 0) will use InitialStep.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static new void DefinePrimitives(PrimitiveEnvironment env)
        {
            const int MaxInt = int.MaxValue;
            env
                .DefinePrimitive(
                        "and", 
                        new[] { "4.2.1", "(and <test1> ...)" },
                        (args, caller) => EvaluateAnd.Call(args, caller.Env, caller), 
                        0, 
                        MaxInt, 
                        Primitive.ArgType.Obj)
                .DefinePrimitive(
                        "begin", 
                        new[] { "4.2.3", "(begin <expression1> <expression2> ...)", "5.2", "(begin <definition1> <definition2> ...)" }, 
                        (args, caller) => EvaluateSequence.Call(args, caller.Env, caller), 
                        0, 
                        MaxInt, 
                        Primitive.ArgType.Obj)
                .DefinePrimitive(
                        "parallel", 
                        new[] { "(parallel <expr> ...)" }, 
                        (args, caller) => EvaluateParallel.Call(args, caller.Env, caller), 
                        0, 
                        MaxInt, 
                        Primitive.ArgType.Pair)
                .DefinePrimitive(
                        "case", 
                        new[] 
                        { 
                            "4.2.1", 
                            "(case <key> <clause1> <clause2> ...)", 
                            "clause: ((<datum1> ...) <expression1> <expression2> ...)", 
                            "else clause: (else <expression1> <expression2> ...)" 
                        }, 
                        (args, caller) => EvaluateCase.Call(args, caller.Env, caller), 
                        0, 
                        MaxInt, 
                        Primitive.ArgType.Pair)
                .DefinePrimitive(
                        "cond", 
                        new[] 
                        { 
                            "4.2.1", 
                            "(cond <clause1> <clause2> ... )", 
                            "clause: (<test> <expression>)", 
                            "clause: (<test> => <recipient>)", 
                            "else clause: (else <expression1> <expression2> ...)" 
                        }, 
                        (args, caller) => EvaluateCond.Call(args, caller.Env, caller), 
                        0, 
                        MaxInt, 
                        Primitive.ArgType.Obj)
                .DefinePrimitive(
                        "define", 
                        new[] 
                        { 
                            "5.2", 
                            "(define <variable> <expression>)", 
                            "(define (<variable> <formals>) <body>)", 
                            "(define (<variable> . <formal>) <body>)" 
                        }, 
                        (args, caller) => EvaluateDefine.Call(args, caller.Env, caller), 
                        0, 
                        MaxInt, 
                        Primitive.ArgType.PairOrSymbol)
                .DefinePrimitive(
                        "do", 
                        new[] { "4.2.4", "(do ((variable1> <init1> <step1>) ...) (<test> <expression> ...) <command> ...)" }, 
                        (args, caller) => EvaluateDo.Call(args, caller.Env, caller), 
                        0, 
                        MaxInt, 
                        Primitive.ArgType.Pair)
                //// Instead of returning a value, return an evaulator that can be run to get the value
                .DefinePrimitive(
                    "eval",
                    new[] { "(eval <expr>)" },
                    (args, caller) => Call(First(args), caller.Env, caller),
                    1,
                    2,
                    Primitive.ArgType.Obj)
                .DefinePrimitive(
                        "if", 
                        new[] { "4.1.5", "(if <test> <consequent> <alternate>)", "(if <test> <consequent>)" }, 
                        (args, caller) => EvaluateIf.Call(args, caller.Env, caller), 
                        0, 
                        MaxInt, 
                        Primitive.ArgType.Obj)
                .DefinePrimitive(
                        "increment!", 
                        new[] { "(increment <variable>)" }, 
                        (args, caller) => Increment(args, caller.Env, caller), 
                        1, 
                        Primitive.ArgType.Symbol)
                .DefinePrimitive(
                        "lambda", 
                        new[] 
                        { 
                            "4.1.4", 
                            "(lambda <formals> <body>)", 
                            "formals: (<variable1> ...)", 
                            "formals: <variable>", 
                            "formals: (<variable 1> ... <variable n-1> . <variable n>)" 
                        },
                        (args, caller) => Lambda.Call(args, caller.Env, caller), 
                        0, 
                        MaxInt, 
                        Primitive.ArgType.PairOrSymbol)
                .DefinePrimitive(
                        "let", 
                        new[] { "4.2.2", "(let <bindings> <body>)", "(let <variable> <bindings> <body>)", "bindings: ((<variable1> <init1>) ...)", "body: <expression> ..." }, 
                        (args, caller) => EvaluateLet.Call(args, caller.Env, caller), 
                        0, 
                        MaxInt, 
                        Primitive.ArgType.PairOrSymbol)
                .DefinePrimitive(
                        "let*", 
                        new[] 
                        { 
                            "4.2.2", 
                            "(let* <bindings> <body>)", 
                            "bindings: ((<variable1> <init1>) ...)", 
                            "body: <expression> ..." 
                        }, 
                        (args, caller) => EvaluateLetStar.Call(args, caller.Env, caller), 
                        0, 
                        MaxInt, 
                        Primitive.ArgType.Pair)
                .DefinePrimitive(
                        "letrec", 
                        new[] { "4.2.2", "(letrec <bindings> <body>)", "bindings: ((<variable1> <init1>) ...)", "body: <expression> ..." }, 
                        (args, caller) => EvaluateLetRec.Call(args, caller.Env, caller), 
                        0, 
                        MaxInt, 
                        Primitive.ArgType.Pair)
                .DefinePrimitive(
                        "macro", 
                        new[] { "(macro (variable1 ...) <body>)" },
                        (args, caller) => Macro.Call(args, caller.Env, caller), 
                        0, 
                        MaxInt, 
                        Primitive.ArgType.Pair)
                .DefinePrimitive(
                        "or", 
                        new[] { "4.2.1", "(or <test1> ...)" }, 
                        (args, caller) => EvaluateOr.Call(args, caller.Env, caller), 
                        0, 
                        MaxInt, 
                        Primitive.ArgType.Obj)
                .DefinePrimitive(
                        "quote", 
                        new[] { "4.1.2", "(quote <datum>)" },
                        EvalQuote, 
                        1, 
                        Primitive.ArgType.Obj)
                .DefinePrimitive(
                        "set!", 
                        new[] { "4.1.6", "(set! <variable> <expression>)" },
                        (args, caller) => EvaluateSet.Call(args, caller.Env, caller),
                        2,
                        Primitive.ArgType.Symbol,
                        Primitive.ArgType.Pair)
                .DefinePrimitive(
                        "time", 
                        new[] { "(time <expr>)" }, 
                        (args, caller) => EvaluateTime.Call(args, caller.Env, caller), 
                        1, 
                        Primitive.ArgType.Obj);
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
        public static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            // If we don't need to do any steps, then
            // do not create an evaluator -- just return the value directly.
            //
            // First look for a symbol.
            if (expr is Symbol)
            {
                var symbol = (Symbol)expr;
                caller.LineNumber = symbol.LineNumber;

                // Evaluate a symbol by looking it up in the environment.
                // It should correspond to a variable name, for which there 
                //    is a corresponding value.
                if (env == null)
                {
                    ErrorHandlers.SemanticError("EvaluateExpression: bad environment", symbol);
                    return null;
                }

                return caller.UpdateReturnValue(env.Lookup(symbol));
            }

            // Look for all other non-pair forms.
            if (!(expr is Pair))
            {
                // If we are evaluating something that is not a pair, 
                //    it must be a constant.
                // Return the integer, real, boolean, or vector.
                return caller.UpdateReturnValue(expr);
            }

            // Break apart and evaluate the fn and args
            // Handle special forms that do not need an evaluator instance.
            var obj = EnsureSchemeObject(expr);
            var fn = First(obj);
            var args = Rest(obj);
            if (fn is Symbol)
            {
                var funSymbol = (Symbol)fn;
                caller.LineNumber = funSymbol.LineNumber;
                var prim = env.Lookup(funSymbol.ToString());
                if (prim is Procedure)
                {
                    var res = ApplySpecial(args, env, caller, (Procedure)prim);
                    if (res != null)
                    {
                        return res;
                    }
                }
            }

            // Actually create an evaluator to do the operation.
            return new EvaluateExpression(args, env, caller, fn);
        }
        #endregion

        #region Private Static Methods
        private static Evaluator ApplySpecial(SchemeObject args, Environment env, Evaluator caller, Procedure fun)
        {
            // TODO must check if args are OK
            // need to get the primitive.  in one case, we have it, in the other, need to get it
            SpecialAction action;
            if (specialActions.TryGetValue(fun.ProcedureName, out action))
            {
                // fun is a primitive
//                return ((Primitive)fun).Apply(args, caller);
                if (!(fun is Primitive))
                {
                    ErrorHandlers.InternalError("special primitive must be primitive");
                }
                var prim = (Primitive)fun;
                // want to dispatch through Primitive.Apply
#if OLD
                return action(args, env, caller);
#else
                return prim.Apply(args, caller);
#endif
            }

            return null;
        }

        /// <summary>
        /// Evaluate a quote expression.
        /// </summary>
        /// <param name="args">The args to quote.</param>
        /// <param name="caller">The caller.</param>
        /// <returns>The quoted expression.</returns>
        private static Evaluator EvalQuote(SchemeObject args, Evaluator caller)
        {
            return caller.UpdateReturnValue(First(args));
        }

        /// <summary>
        /// Evaluate a quote expression.
        /// </summary>
        /// <param name="args">The args to quote.</param>
        /// <param name="env">The environment (unused).</param>
        /// <param name="caller">The caller.</param>
        /// <returns>The quoted expression.</returns>
        private static Evaluator EvalQuote(SchemeObject args, Environment env, Evaluator caller)
        {
            return caller.UpdateReturnValue(First(args));
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

            // If we get here, it wasn't one of the special forms.  
            // So we need to evaluate the first item (the function) in preparation for
            //    doing a procedure call.
            //// <r4rs section="4.1.3">(<operator> <operand1> ...)</r4rs>
            return Call(step.fn, s.Env, s.ContinueAt(ApplyProcStep));
        }

        /// <summary>
        /// Increment the variable.
        /// The intention is that this should be atomic.
        /// </summary>
        /// <param name="expr">The symbol whose value is incremented.</param>
        /// <param name="env">The environment.</param>
        /// <param name="caller">Return to this caller.</param>
        /// <returns>The next step to execute.</returns>
        private static Evaluator Increment(SchemeObject expr, Environment env, Evaluator caller)
        {
            SchemeObject lhs = First(expr);
            if (!(lhs is Symbol))
            {
                ErrorHandlers.SemanticError(string.Format(@"Increment: first argument must be a symbol.  Got: ""{0}""", lhs), null);
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
            var fn = s.ReturnedExpr;
            if (!(fn is Procedure))
            {
                ErrorHandlers.SemanticError(string.Format(@"Value must be procedure: ""{0}""", fn), null);
            }

            var proc = (Procedure)fn;

            // detect if fn is a "special" primitive that does not get its args evaluated, and if so apply
            var res = ApplySpecial(s.Expr, s.Env, s.Caller, proc);
            if (res != null)
            {
                return res;
            }

            // call normal evaluator 
            return proc.Evaluate(s.Expr, s.Env, s.Caller);
        }

        #endregion
    }
}
