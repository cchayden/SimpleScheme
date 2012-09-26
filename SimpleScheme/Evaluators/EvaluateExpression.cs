// <copyright file="EvaluateExpression.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Diagnostics.Contracts;

    #region Delegtes
    /// <summary>
    /// Primitives that do not evaluate their arguments can be handled more efficiently.
    /// Instead of creating a new EvaluateExpression object, we can just execute an action
    /// directly.  This defines the form of those special actions.
    /// </summary>
    /// <param name="args">The primitive arguments.</param>
    /// <param name="env">The environment in which to execute the primitive.</param>
    /// <param name="caller">The caller, to return to.</param>
    /// <returns>The next evaluator to run.</returns>
    internal delegate Evaluator SpecialAction(SchemeObject args, Environment env, Evaluator caller);
    #endregion

    /// <summary>
    /// The main evaluator for expressions.
    /// </summary>
   //// <r4rs section="4.1.3">(<operator> <operand1> ...)</r4rs>
    internal sealed class EvaluateExpression : Evaluator
    {
        #region Fields
        /// <summary>
        /// This is used to detect symbols that can be handled specially.
        /// </summary>
        private static readonly Dictionary<string, SpecialAction> specialActions;

        /// <summary>
        /// The function to evaluate.
        /// </summary>
        private SchemeObject fn;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes static members of the EvaluateExpression class.
        /// These are all primitives that do not evaluate their arguments.
        /// If we can quickly recognize these we can avoid creating an EvaluateExpression object.
        /// We cannot always recognize these, so we have to be prepared to evaluate them in the normal way as well.
        /// For better performance, once we see one of these, we stash the action into the symbol representing the primitive.
        /// We cannot do this at parse time, because the symbol may not be a primitive in that context.
        /// </summary>
        static EvaluateExpression()
        {
            specialActions = new Dictionary<string, SpecialAction>
                {
                    { "and", EvaluateAnd.Call },
                    { "begin", EvaluateSequence.Call },
                    { "parallel", EvaluateParallel.Call },
                    { "case", EvaluateCase.Call },
                    { "cond", EvaluateCond.Call },
                    { "define", EvaluateDefine.Call },
                    { "do", EvaluateDo.Call },
                    { "if", EvaluateIf.Call },
                    { "increment!", Increment },
                    { "lambda", Lambda.Call },
                    { "let", EvaluateLet.Call },
                    { "let*", EvaluateLetStar.Call },
                    { "letrec", EvaluateLetRec.Call },
                    { "macro", Macro.Call },
                    { "or", EvaluateOr.Call },
                    { "quote", EvalQuote },
                    { "set!", EvaluateSet.Call },
                    { "time", EvaluateTime.Call }
                };
        }
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
        /// <param name="primEnv">The environment to define the primitives into.</param>
        internal static new void DefinePrimitives(PrimitiveEnvironment primEnv)
        {
            Contract.Requires(primEnv != null);
            primEnv
                .DefinePrimitive(
                        "and",
                        new[] { "4.2.1", "(and <test1> ...)" },
                        EvaluateAnd.Call,
                        new ArgsInfo(true, ArgType.Obj))
                .DefinePrimitive(
                        "begin",
                        new[] { "4.2.3", "(begin <expression1> <expression2> ...)", "5.2", "(begin <definition1> <definition2> ...)" },
                        EvaluateSequence.Call,
                        new ArgsInfo(true, ArgType.Obj))
                .DefinePrimitive(
                        "parallel",
                        new[] { "(parallel <expr> ...)" },
                        EvaluateParallel.Call,
                        new ArgsInfo(true, ArgType.Obj))   // *** was Pair
                .DefinePrimitive(
                        "case",
                        new[] 
                        { 
                            "4.2.1", 
                            "(case <key> <clause1> <clause2> ...)", 
                            "clause: ((<datum1> ...) <expression1> <expression2> ...)", 
                            "else clause: (else <expression1> <expression2> ...)" 
                        },
                        EvaluateCase.Call,
                        new ArgsInfo(true, ArgType.Pair))
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
                        EvaluateCond.Call,
                        new ArgsInfo(true, ArgType.Obj))
                .DefinePrimitive(
                        "define",
                        new[] 
                        { 
                            "5.2", 
                            "(define <variable> <expression>)", 
                            "(define (<variable> <formals>) <body>)", 
                            "(define (<variable> . <formal>) <body>)" 
                        },
                        EvaluateDefine.Call,
                        new ArgsInfo(true, ArgType.PairOrSymbol, ArgType.Obj))
                .DefinePrimitive(
                        "do",
                        new[] { "4.2.4", "(do ((variable1> <init1> <step1>) ...) (<test> <expression> ...) <command> ...)" },
                        EvaluateDo.Call,
                        new ArgsInfo(true, ArgType.Pair))
                .DefinePrimitive(
                        "if",
                        new[] { "4.1.5", "(if <test> <consequent> <alternate>)", "(if <test> <consequent>)" },
                        EvaluateIf.Call,
                        new ArgsInfo(true, ArgType.Obj))
                .DefinePrimitive(
                        "increment!",
                        new[] { "(increment <variable>)" },
                        Increment,
                        new ArgsInfo(1, true, ArgType.Symbol))
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
                        Lambda.Call,
                        new ArgsInfo(true, ArgType.PairOrSymbolOrEmpty, ArgType.Obj)) // *** Was PairOrSymbol  (empty list permitted)
                .DefinePrimitive(
                        "let",
                        new[]
                            {
                                "4.2.2", 
                                "(let <bindings> <body>)", 
                                "(let <variable> <bindings> <body>)", 
                                "bindings: ((<variable1> <init1>) ...)", 
                                "body: <expression> ..."
                            },
                        EvaluateLet.Call,
                        new ArgsInfo(true, ArgType.PairOrSymbolOrEmpty, ArgType.Obj))
                .DefinePrimitive(
                        "let*",
                        new[] 
                        { 
                            "4.2.2", 
                            "(let* <bindings> <body>)", 
                            "bindings: ((<variable1> <init1>) ...)", 
                            "body: <expression> ..." 
                        },
                        EvaluateLetStar.Call,
                        new ArgsInfo(true, ArgType.PairOrEmpty, ArgType.Obj))
                .DefinePrimitive(
                        "letrec",
                        new[] { "4.2.2", "(letrec <bindings> <body>)", "bindings: ((<variable1> <init1>) ...)", "body: <expression> ..." },
                        EvaluateLetRec.Call,
                        new ArgsInfo(true, ArgType.PairOrEmpty, ArgType.Obj))
                .DefinePrimitive(
                        "macro",
                        new[] { "(macro (variable1 ...) <body>)" },
                        Macro.Call,
                        new ArgsInfo(true, ArgType.Pair))
                .DefinePrimitive(
                        "or",
                        new[] { "4.2.1", "(or <test1> ...)" },
                        EvaluateOr.Call,
                        new ArgsInfo(true, ArgType.Obj))
                .DefinePrimitive(
                        "quote",
                        new[] { "4.1.2", "(quote <datum>)" },
                        EvalQuote,
                        new ArgsInfo(1, true, ArgType.Obj))
                .DefinePrimitive(
                        "set!",
                        new[] { "4.1.6", "(set! <variable> <expression>)" },
                        EvaluateSet.Call,
                        new ArgsInfo(2, true, ArgType.Symbol, ArgType.Obj))
                .DefinePrimitive(
                        "time",
                        new[] { "(time <expr>)" },
                        EvaluateTime.Call,
                        new ArgsInfo(1, true, ArgType.Obj))
                //// Instead of returning a value, return an evaulator that can be run to get the value
                .DefinePrimitive(
                        "eval",
                        new[] { "(eval <expr>)" },
                        (args, env, caller) => Call(First(args), caller.Env, caller),
                        new ArgsInfo(1, 2, ArgType.Obj));
        }
        #endregion

        #region Call
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
        internal static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Ensures(Contract.Result<Evaluator>() != null);

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
                Debug.Assert(env != null, "EvaluateExpression:Call env is null");
                caller.ReturnedExpr = env.Lookup(symbol);
                return caller;
            }

            // Look for all other non-pair forms.
            if (!(expr is Pair))
            {
                // If we are evaluating something that is not a pair, 
                //    it must be a constant.
                // Return the integer, real, boolean, or vector.
                caller.ReturnedExpr = expr;
                return caller;
            }

            // Break apart and evaluate the fn and args
            // Handle special forms that do not need an evaluator instance.
            var obj = expr;
            var fn = First(obj);
            var args = Rest(obj);
            if (fn is Symbol)
            {
                var funSymbol = (Symbol)fn;
                caller.LineNumber = funSymbol.LineNumber;
                SpecialAction action = funSymbol.SpecialForm;
                if (action != null)
                {
                    var res = action(args, env, caller);
                    Contract.Assume(res != null);
                    return res;
                }

                if (specialActions.TryGetValue(funSymbol.ToString(), out action))
                {
                    funSymbol.SpecialForm = action;
                    var res = action(args, env, caller);
                    Contract.Assume(res != null);
                    return res;
                }

                // If fn is still a symbol, then it needs its arguments evaluated
                // and so it needs an instance of EvaluateExpression to manage that.
            }

            // Actually create an evaluator to do the operation.
            return New(expr, env, caller);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Start evaluation by testing the various forms.
        /// The special forms have their own evaluators.
        /// Otherwise, evaluate the first argument (the proc) in preparation for a call.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator InitialStep()
        {
            // If we get here, it wasn't one of the special forms.  
            // So we need to evaluate the first item (the function) in preparation for
            //    doing a procedure call.
            //// <r4rs section="4.1.3">(<operator> <operand1> ...)</r4rs>
            this.Pc = OpCode.ApplyProc;
            return Call(this.fn, this.Env, this);
        }

        /// <summary>
        /// Come here after evaluating the first expression, the proc.
        /// Handle the proc: macro, lambda, or function call.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator ApplyProcStep()
        {
            // Come here after evaluating fn
            var ret = this.ReturnedExpr;
            if (!(ret is Procedure))
            {
                ErrorHandlers.SemanticError(string.Format(@"Value must be procedure: ""{0}""", ret));
            }

            var proc = (Procedure)ret;
            Contract.Assert(proc != null);

            // detect if proc is a "special" primitive that does not get its args evaluated, and if so apply
            var res = ApplySpecial(this.Expr, this.Env, this.Caller, proc);
            if (res != null)
            {
                return res;
            }

            // call normal evaluator 
            SchemeObject exp = this.Expr;
            Environment ev = this.Env;
            Evaluator c = this.Caller;
            this.Reclaim();
            return proc.Evaluate(exp, ev, c);
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Test the fun to apply to see if it is a special procedure.
        /// The special procedures do not evaluate their arguments.
        /// These must be implemented as primitives.
        /// </summary>
        /// <param name="args">The args to apply.</param>
        /// <param name="env">The environment in which to do the application.</param>
        /// <param name="caller">The caller to return to.</param>
        /// <param name="fun">The procedure to apply.</param>
        /// <returns>An evaluator.</returns>
        private static Evaluator ApplySpecial(SchemeObject args, Environment env, Evaluator caller, Procedure fun)
        {
            Contract.Requires(args != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(fun != null);
            if (!fun.EvaluateArgs)
            {
                Contract.Assume(fun is Primitive);
                return ((Primitive)fun).Apply(args, env, caller);                
            }

            return null;
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
            Contract.Requires(args != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            caller.ReturnedExpr = First(args);
            return caller;
        }

        /// <summary>
        /// Increment the variable.
        /// The intention is that this should be atomic.
        /// Used by increment! primitive.
        /// </summary>
        /// <param name="expr">The symbol whose value is incremented.</param>
        /// <param name="env">The environment.</param>
        /// <param name="caller">Return to this caller.</param>
        /// <returns>The next step to execute.</returns>
        private static Evaluator Increment(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            SchemeObject lhs = First(expr);
            if (!(lhs is Symbol))
            {
                ErrorHandlers.SemanticError(string.Format(@"Increment: first argument must be a symbol.  Got: ""{0}""", lhs));
            }

            caller.ReturnedExpr = env.Increment(lhs);
            return caller;
        }
        #endregion

        #region Initialize
        /// <summary>
        /// Creates and initializes a new instance of the EvaluateExpression class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Initialized evaluator.</returns>
        private static EvaluateExpression New(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return GetInstance<EvaluateExpression>().Initialize(expr, env, caller);
        }

        /// <summary>
        /// Initializes a new instance of the EvaluateExpression class.
        /// At this point, fn is something that evaluates to a proc.
        /// It might be a symbol that, when looked up, resolves to a primitive or to a
        ///   proc of some kind, like a lambda.
        /// It is not a symbol like "and" where there are special argument-evaluation rules, so
        ///   the first task is to evaluate all the arguments.
        /// </summary>
        /// <param name="expr">The function and the arguments to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Newly initialized evaluator.</returns>
        private EvaluateExpression Initialize(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            var args = Rest(expr);
            this.fn = First(expr);
            Initialize(OpCode.Initial, args, env, caller);
            if (fn is Procedure)
            {
                // If the fun is already a procedure, skip to apply step
                this.Pc = OpCode.ApplyProc;
                this.ReturnedExpr = this.fn;
            }

            return this;
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.degenerate || this.fn != null);
        }
        #endregion
    }
}
