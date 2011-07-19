// <copyright file="Interpreter.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using Obj = System.Object;

    /// <summary>
    /// The scheme interpreter instance.
    /// Each one of these is a complete interpreter, independent of others.
    /// </summary>
    public sealed class Interpreter : IInterpreter
    {
        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("step");

        /// <summary>
        /// The initial step.  When complete, evaluation is done.
        /// </summary>
        private readonly Stepper halted;

        /// <summary>
        /// The async result used in case the interpreter is called asynchronously.
        /// </summary>
        private AsyncResult<object> asyncResult;
        #endregion

        #region Constructors
        /// <summary>
        /// Initializes a new instance of the Interpreter class.
        /// Create an interpreter and install the primitives into the global environment.
        /// Then read a list of files.
        /// </summary>
        /// <param name="loadStandardMacros">Load standard macros and other primitives.</param>
        /// <param name="primEnvironment">Environment containing the primitives (can be null).</param>
        /// <param name="files">The files to read.</param>
        /// <param name="reader">The input reader.</param>
        /// <param name="writer">The output writer.</param>
        private Interpreter(bool loadStandardMacros, PrimitiveEnvironment primEnvironment, IEnumerable<string> files, TextReader reader, TextWriter writer)
        {
            this.Trace = false;
            this.Count = false;
            this.Input = InputPort.New(reader ?? Console.In);
            this.Output = OutputPort.New(writer ?? Console.Out);
            this.PrimEnvironment = primEnvironment ?? PrimitiveEnvironment.New();

            this.Counters = new Counter();
            this.GlobalEnvironment = Environment.NewGlobal(this, PrimEnvironment);
            this.halted = new EvaluatorBase("halted", this.GlobalEnvironment, null);

            try
            {
                if (loadStandardMacros)
                {
                    this.LoadString(SchemePrimitives.Code);
                }

                if (files != null)
                {
                    foreach (string file in files)
                    {
                        this.LoadFile(file);
                    }
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine("Caught exception {0}", ex.Message);
            }
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the global environment.
        /// For this accessor, the environment is exposed as the environment interface.
        /// </summary>
        public IEnvironment GlobalEnv
        {
            get { return this.GlobalEnvironment; }
        }

        public IPrimitiveEnvironment PrimEnv
        {
            get { return this.PrimEnvironment; }
        }

        /// <summary>
        /// Gets the global environment for the interpreter.
        /// </summary>
        internal Environment GlobalEnvironment { get; private set; }

        /// <summary>
        /// Gets the rimitive environment for the interpreter.
        /// </summary>
        internal PrimitiveEnvironment PrimEnvironment { get; private set; }

        /// <summary>
        /// Gets the input port.
        /// </summary>
        internal InputPort Input { get; private set; }

        /// <summary>
        /// Gets the output port.
        /// </summary>
        internal OutputPort Output { get; private set; }

        /// <summary>
        /// Gets the counters collection.
        /// </summary>
        internal Counter Counters { get; private set; }

        /// <summary>
        /// Gets or sets a value indicating whether to trace.
        /// </summary>
        internal bool Trace { get; set; }

        /// <summary>
        /// Gets or sets a value indicating whether to count.
        /// </summary>
        internal bool Count { get; set; }
        #endregion

        #region Public Methods
        /// <summary>
        /// Create a new interpeter with a given set of files to run initially.
        /// </summary>
        /// <param name="loadStandardMacros">Load standard macros and other primitives.</param>
        /// <param name="primEnvironment">Environment containing the primitives.  If null, create one.</param>
        /// <param name="files">The files to read.</param>
        /// <param name="reader">The input reader.</param>
        /// <param name="writer">The output writer.</param>
        /// <returns>A scheme interpreter.</returns>
        public static Interpreter New(bool loadStandardMacros, IPrimitiveEnvironment primEnvironment, IEnumerable<string> files, TextReader reader, TextWriter writer)
        {
            PrimitiveEnvironment primEnv = primEnvironment as PrimitiveEnvironment ?? PrimitiveEnvironment.New();
            return new Interpreter(loadStandardMacros, primEnv, files, reader, writer);
        }

        /// <summary>
        /// Create a new interpeter with a given set of files to run initially.
        /// </summary>
        /// <param name="primEnvironment">Environment containing the primitives.  If null, create one.</param>
        /// <returns>A scheme interpreter.</returns>
        public static Interpreter New(IPrimitiveEnvironment primEnvironment)
        {
            PrimitiveEnvironment primEnv = primEnvironment as PrimitiveEnvironment ?? PrimitiveEnvironment.New();
            return new Interpreter(true, primEnv, null, null, null);
        }

        /// <summary>
        /// Create a new interpeter with a given set of files to run initially.
        /// </summary>
        /// <param name="files">The files to read.</param>
        /// <returns>A scheme interpreter.</returns>
        public static Interpreter New(IEnumerable<string> files)
        {
            return new Interpreter(true, null, files, null, null);
        }

        /// <summary>
        /// Create a new interpreter with standard macros and no files to load.
        /// </summary>
        /// <returns>A scheme interpreter</returns>
        public static Interpreter New()
        {
            return new Interpreter(true, null, null, null, null);
        }

        /// <summary>
        /// Evaluate an expression (expressed as a list) in the global environment.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The result of the evaluation.</returns>
        public Obj Eval(Obj expr)
        {
            return this.Eval(expr, this.GlobalEnvironment);
        }

        /// <summary>
        /// Begin an asynchronous evaluation
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="cb">Call this when evaluation is complete.</param>
        /// <param name="state">Pass this through for the callback function.</param>
        /// <returns>Async result, used to monitor progress.</returns>
        public IAsyncResult BeginEval(Obj expr, AsyncCallback cb, object state)
        {
            this.asyncResult = new AsyncResult<object>(cb, state);
            Obj res = this.Eval(expr, this.GlobalEnvironment);
            if (res == Stepper.Suspended)
            {
                return this.asyncResult;
            }

            if (!this.asyncResult.IsCompleted)
            {
                this.asyncResult.SetAsCompleted(res, true);
            }

            return this.asyncResult;
        }

        /// <summary>
        /// End asynchronous evaluation and get the result.
        /// </summary>
        /// <param name="ar">Async result from callback.</param>
        /// <returns>The expression value.</returns>
        public object EndEval(IAsyncResult ar)
        {
            return ((AsyncResult<object>)ar).EndInvoke();
        }

        /// <summary>
        /// Read an expression, evaluate it, and print the results.
        /// </summary>
        /// <returns>If end of file, InputPort.Eof, otherwise the IAsyncResult.</returns>
        public IAsyncResult ReadEvalPrintAsync()
        {
            try
            {
                Obj expr;
                this.Output.Print("> ");
                this.Output.Flush();
                if (InputPort.IsEof(expr = this.Input.ReadObj()))
                {
                    return new CompletedAsyncResult<string>(InputPort.Eof);
                }

                return this.BeginEval(
                    expr,
                    ar =>
                        {
                            object val = this.EndEval(ar);
                            if (val != Undefined.Instance)
                            {
                                this.Output.Write(val);
                                this.Output.Println();
                                this.Output.Flush();
                            }
                        },
                    null);
            }
            catch (Exception ex)
            {
                Console.WriteLine("Caught exception {0}", ex.Message);
                return null;
            }
        }

        /// <summary>
        /// Read from an input port, evaluate in the global environment, and print the result.
        /// If value is undefined, do not print anything.
        /// If the result is suspended, keep on going with another expression.
        /// This will then execute both expressions asynchronously.
        /// Catch and discard exceptions.
        /// </summary>
        /// <returns>The result of the last evaluation.</returns>
        public Obj ReadEvalPrintLoop()
        {
            Obj prevExpr = null;
            while (true)
            {
                Obj expr = this.ReadEvalPrint();
                if (ReferenceEquals(expr, InputPort.Eof))
                {
                    return prevExpr;
                }

                prevExpr = expr;
            }
        }

        /// <summary>
        /// Load a file.  
        /// Open the file and read it.
        /// Evaluate whatever it contains.
        /// </summary>
        /// <param name="fileName">The filename.</param>
        public void LoadFile(object fileName)
        {
            string name = Printer.AsString(fileName, false);
            try
            {
                using (var fs = new FileStream(name, FileMode.Open, FileAccess.Read))
                {
                    this.Load(InputPort.New(new StreamReader(fs)));
                }
            }
            catch (IOException)
            {
                ErrorHandlers.IoError("Can't load " + name);
            }
        }

        /// <summary>
        /// Read from the given port and evaluate the expression.
        /// </summary>
        /// <param name="inp">The input port to read from.</param>
        /// <returns>The result of the evaluation.</returns>
        public Obj ReadEval(InputPort inp)
        {
            try
            {
                Obj expr;
                if (InputPort.IsEof(expr = inp.ReadObj()))
                {
                    return InputPort.Eof;
                }

                Obj val = this.Eval(expr);
                return val;
            }
            catch (Exception ex)
            {
                Console.WriteLine("Caught exception {0}", ex.Message);
                return Undefined.Instance;
            }
        }

        /// <summary>
        /// Read from a string and evaluate.
        /// This is done for the side effects, not the value.
        /// </summary>
        /// <param name="str">The string to read and evaluate.</param>
        public void LoadString(string str)
        {
            using (StringReader reader = new StringReader(str))
            {
                this.Load(InputPort.New(reader));
            }
        }

        /// <summary>
        /// Evaluate a string and return the result.
        /// </summary>
        /// <param name="str">The program to evaluate.</param>
        /// <returns>The evaluation result.</returns>
        public Obj EvalString(string str)
        {
            using (StringReader reader = new StringReader(str))
            {
                return this.ReadEval(InputPort.New(reader));
            }
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the counter primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        internal static void DefinePrimitives(PrimitiveEnvironment env)
        {
            env
                //// (trace-on)
                .DefinePrimitive("trace-on", (args, caller) => SetTrace(caller, true), 0)
                //// (trace-off)
                .DefinePrimitive("trace-off", (args, caller) => SetTrace(caller, false), 0)
                //// (counters-on)
                .DefinePrimitive("counters-on", (args, caller) => SetCount(caller, true), 0)
                //// (counters-off)
                .DefinePrimitive("counters-off", (args, caller) => SetCount(caller, false), 0)
                //// (backtrace)
                .DefinePrimitive("backtrace", (args, caller) => Backtrace(caller), 0);
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Evaluate an expression in an environment.
        /// Do it by executing a set of steps.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment in which to evaluate it.</param>
        /// <returns>The result of the evaluation.</returns>
        internal Obj Eval(Obj expr, Environment env)
        {
            return this.EvalStep(EvaluateExpression.Call(expr, env, this.halted));
        }

        /// <summary>
        /// Perform steps until evaluation is complete or suspended.
        /// After suspension, return to this entry point.
        /// </summary>
        /// <param name="step">The step to perform first.</param>
        /// <returns>The evaluation result, or suspended stepper.</returns>
        internal Obj EvalStep(Stepper step)
        {
            while (true)
            {
                if (step == Stepper.Suspended)
                {
                    // TODO should this return the async result?
                    return step;
                }

                if (step == this.halted)
                {
                    if (this.asyncResult != null)
                    {
                        this.asyncResult.SetAsCompleted(this.halted.ReturnedExpr, false);
                    }

                    return this.halted.ReturnedExpr;
                }

                if (step == null)
                {
                    return ErrorHandlers.InternalError("PC bad value");
                }

                this.TraceStep(step);
                step.IncrementCounter(counter);

                // run the step and capture the next step
                step = step.RunStep();
            }
        }

        /// <summary>
        /// Read from the input port and evaluate whatever is there.
        /// Done for the side effect, not the result.
        /// Since the result is never tested, asynchronous suspensions do not prevent the rest
        /// of the file from being loaded.
        /// </summary>
        /// <param name="inp">The input port.</param>
        /// <returns>Undefined instance.</returns>
        internal Obj Load(InputPort inp)
        {
            while (true)
            {
                Obj input;
                if (InputPort.IsEof(input = inp.ReadObj()))
                {
                    inp.Close();
                    return Undefined.Instance;
                }

                this.Eval(input);
            }
        }

        /// <summary>
        /// Read an expression, evaluate it, and print the results.
        /// </summary>
        /// <returns>If end of file, InputPort.Eof, otherwise expr.</returns>
        internal Obj ReadEvalPrint()
        {
            try
            {
                Obj expr;
                this.Output.Print("> ");
                this.Output.Flush();
                if (InputPort.IsEof(expr = this.Input.ReadObj()))
                {
                    return InputPort.Eof;
                }

                Obj val = this.Eval(expr);
                if (val != Undefined.Instance)
                {
                    this.Output.Write(val);
                    this.Output.Println();
                    this.Output.Flush();
                }

                return val;
            }
            catch (Exception ex)
            {
                Console.WriteLine("Caught exception {0}", ex.Message);
                return Undefined.Instance;
            }
        }

        /// <summary>
        /// Increment the given counter.
        /// Skip if if counting is turned off.
        /// </summary>
        /// <param name="counterId">The counter to increment.</param>
        internal void IncrementCounter(int counterId)
        {
            if (this.Count)
            {
                this.Counters.Increment(counterId);
            }
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Sets tracing on or off.
        /// </summary>
        /// <param name="caller">The calling stepper.</param>
        /// <param name="flag">The new trace state.</param>
        /// <returns>Undefined object.</returns>
        private static Obj SetTrace(Stepper caller, bool flag)
        {
            caller.TraceFlag = flag;
            return Undefined.Instance;
        }

        /// <summary>
        /// Sets counting on or off.
        /// </summary>
        /// <param name="caller">The calling stepper.</param>
        /// <param name="flag">The new count state.</param>
        /// <returns>Undefined object.</returns>
        private static Obj SetCount(Stepper caller, bool flag)
        {
            caller.CountFlag = flag;
            return Undefined.Instance;
        }

        /// <summary>
        /// Display a stack backtrace.
        /// </summary>
        /// <param name="caller">The caller.</param>
        /// <returns>Undefined result.</returns>
        private static Obj Backtrace(Stepper caller)
        {
            caller.CurrentOutputPort.Outp.WriteLine(caller.StackBacktrace());
            return Undefined.Instance;
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Write trace info if trace enabled.
        /// </summary>
        /// <param name="step">The step to trace.</param>
        private void TraceStep(Stepper step)
        {
            if (!this.Trace)
            {
                return;
            }

            string info = step.TraceInfo();
            if (info == null)
            {
                return;
            }

            step.CurrentOutputPort.Outp.WriteLine("{0}: {1}", info, step.Expr);
        }
        #endregion
    }
}