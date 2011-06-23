// <copyright file="ActivationRecord.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluator contains all the individual evaluators
    /// </summary>
    public partial class Evaluator
    {
        private class ActivationRecord
        {
            public ActivationRecord(Scheme interp, Evaluator parent, object expr, Environment env)
            {
                this.Interp = interp;
                this.Parent = parent;
                this.RetExpr = this.Expr = expr;
                this.RetEnv = this.Env = env;
                this.Pc = 0;
                this.Called = null;
            }

            /// <summary>
            /// The scheme interpreter.
            /// </summary>
            public Scheme Interp { get; private set; }

            /// <summary>
            /// The parent that execution returns to when this is done.
            /// </summary>
            public Evaluator Parent { get; private set; }

            /// <summary>
            /// Gets or sets the expression being evaluated.  
            /// </summary>
            public object Expr { get; set; }

            /// <summary>
            /// Gets or sets the returned expression.
            /// This is valid after a call has completed, and holds the
            ///   returned result.
            /// </summary>
            public object RetExpr { get; set; }

            /// <summary>
            /// Gets or sets the evaluation environment.  After execution, this is the new environment.
            /// </summary>
            public Environment Env { get; set; }

            /// <summary>
            /// Gets or sets the returned environment
            /// </summary>
            public Environment RetEnv { private get; set; }

            /// <summary>
            /// Gets or sets the Evaluators program counter.
            /// Used to sequence through multiple steps.
            /// </summary>
            public int Pc { get; set; }

            /// <summary>
            /// Gets or sets the called sub-evaluator.
            /// The evaluator that is called is stored, so that the returned value
            ///   can be extracted after it returns.
            /// </summary>
            public Evaluator Called { private get; set; }

            /// <summary>
            /// Gets the returned expression from the last call.
            /// </summary>
            public object CalledRetExpr
            {
                get { return Called.Frame.RetExpr; }
            }

            /// <summary>
            /// Gets the returned environment from the last call.
            /// </summary>
            public Environment CalledRetEnv
            {
                get { return Called.Frame.RetEnv; }
            }
        }
    }
}