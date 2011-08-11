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
        #region Constants
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("eval");

        /// <summary>
        /// The interactive prompt.
        /// </summary>
        private const string Prompt = "> ";
        #endregion

        #region Fields
        /// <summary>
        /// The initial step.  When complete, evaluation is done.
        /// </summary>
        private readonly Evaluator halted;

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
            this.Transcript = new TranscriptLogger(this);
            this.CurrentInputPort = new InputPort(reader ?? Console.In, this);
            this.CurrentOutputPort = new OutputPort(writer ?? Console.Out, this);
            this.PrimEnvironment = primEnvironment ?? new PrimitiveEnvironment();

            this.CurrentCounters = new Counter();
            this.GlobalEnvironment = new Environment(this, this.PrimEnvironment);
            this.halted = Evaluator.NewHalted(this.GlobalEnvironment);

            try
            {
                if (loadStandardMacros)
                {
                    this.Load(SchemePrimitives.Code);
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
                ErrorHandlers.PrintException(ex);
            }
        }

        /// <summary>
        /// Initializes a new instance of the Interpreter class.
        /// Create a new interpeter with a given set of files to run initially, a given environment, and given streams..
        /// </summary>
        /// <param name="loadStandardMacros">Load standard macros and other primitives.</param>
        /// <param name="primEnvironment">Environment containing the primitives.  If null, create one.</param>
        /// <param name="files">The files to read.</param>
        /// <param name="reader">The input reader.</param>
        /// <param name="writer">The output writer.</param>
        /// <returns>A scheme interpreter.</returns>
        private Interpreter(bool loadStandardMacros, IPrimitiveEnvironment primEnvironment, IEnumerable<string> files, TextReader reader, TextWriter writer) :
            this(loadStandardMacros, primEnvironment as PrimitiveEnvironment, files, reader, writer)
        {
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

        /// <summary>
        /// Gets the primitive environment.
        /// </summary>
        public IPrimitiveEnvironment PrimEnv
        {
            get { return this.PrimEnvironment; }
        }

        /// <summary>
        /// Gets the halted evaluator.
        /// </summary>
        public Evaluator Halted
        {
            get { return this.halted; }
        }

        /// <summary>
        /// Gets the global environment for the interpreter.
        /// </summary>
        public Environment GlobalEnvironment { get; private set; }

        /// <summary>
        /// Gets the rimitive environment for the interpreter.
        /// </summary>
        public PrimitiveEnvironment PrimEnvironment { get; private set; }

        /// <summary>
        /// Gets the input port.
        /// </summary>
        public InputPort CurrentInputPort { get; private set; }

        /// <summary>
        /// Gets the output port.
        /// </summary>
        public OutputPort CurrentOutputPort { get; private set; }

        /// <summary>
        /// Gets the counters collection.
        /// </summary>
        public Counter CurrentCounters { get; private set; }

        /// <summary>
        /// Gets or sets a value indicating whether to trace.
        /// </summary>
        public bool Trace { get; set; }

        /// <summary>
        /// Gets or sets a value indicating whether to count.
        /// </summary>
        public bool Count { get; set; }

        /// <summary>
        /// Gets the transcript logger.
        /// </summary>
        public TranscriptLogger Transcript { get; private set; }
        #endregion

        #region Public Factory Methods
        /// <summary>
        /// Create a new interpreter with all the default settings.
        /// </summary>
        /// <returns>An interpreter.</returns>
        public static IInterpreter New()
        {
            return new Interpreter(true, null, null, null, null);
        }

        /// <summary>
        /// Create a new interpreter with a given set of files to run initially.
        /// </summary>
        /// <param name="files">The files to read.</param>
        /// <returns>A scheme interpreter.</returns>
        public static IInterpreter New(IEnumerable<string> files)
        {
            return new Interpreter(true, null, files, null, null);
        }

        /// <summary>
        /// Create a new interpeter with a given set of files to run initially, a given environment, and given streams..
        /// </summary>
        /// <param name="primEnvironment">Environment containing the primitives.  If null, create one.</param>
        /// <returns>A scheme interpreter.</returns>
        public static IInterpreter New(IPrimitiveEnvironment primEnvironment)
        {
            return new Interpreter(true, primEnvironment, null, null, null);
        }

        /// <summary>
        /// Create a new interpeter with a given set of files to run initially, a given environment, and given streams..
        /// </summary>
        /// <param name="loadStandardMacros">Load standard macros and other primitives.</param>
        /// <param name="primEnvironment">Environment containing the primitives.  If null, create one.</param>
        /// <param name="files">The files to read.</param>
        /// <param name="reader">The input reader.</param>
        /// <param name="writer">The output writer.</param>
        /// <returns>A scheme interpreter.</returns>
        public static IInterpreter New(bool loadStandardMacros, IPrimitiveEnvironment primEnvironment, IEnumerable<string> files, TextReader reader, TextWriter writer)
        {
            return new Interpreter(loadStandardMacros, primEnvironment, files, reader, writer);
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the counter primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(PrimitiveEnvironment env)
        {
            env
                //// (trace-on)
                .DefinePrimitive("trace-on", (args, caller) => SetTraceFlag(caller, true), 0)
                //// (trace-off)
                .DefinePrimitive("trace-off", (args, caller) => SetTraceFlag(caller, false), 0)
                //// (counters-on)
                .DefinePrimitive("counters-on", (args, caller) => SetCountFlag(caller, true), 0)
                //// (counters-off)
                .DefinePrimitive("counters-off", (args, caller) => SetCountFlag(caller, false), 0)
                //// (backtrace)
                .DefinePrimitive("backtrace", (args, caller) => Backtrace(caller), 0);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Evaluate an expression (expressed as a list) in the global environment.
        /// Catch any exceptions that may happen.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The result of the evaluation.</returns>
        public Obj Eval(Obj expr)
        {
            try
            {
                return this.UnsafeEval(expr);
            }
            catch (Exception ex)
            {
                ErrorHandlers.PrintException(ex);
                return Undefined.Instance;
            }
        }

        /// <summary>
        /// Begin an asynchronous evaluation.
        /// Catch any exceptions that might happen.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="cb">Call this when evaluation is complete.</param>
        /// <param name="state">Pass this through for the callback function.</param>
        /// <returns>Async result, used to monitor progress.</returns>
        public IAsyncResult BeginEval(Obj expr, AsyncCallback cb, object state)
        {
            try
            {
                return this.UnsafeBeginEval(expr, cb, state);
            }
            catch (Exception ex)
            {
                ErrorHandlers.PrintException(ex);
                return null;
            }
        }

        /// <summary>
        /// End asynchronous evaluation and get the result.
        /// Catch any exceptions that might happen.
        /// </summary>
        /// <param name="ar">Async result from callback.</param>
        /// <returns>The expression value.</returns>
        public object EndEval(IAsyncResult ar)
        {
            try
            {
                return this.UnsafeEndEval(ar);
            }
            catch (Exception ex)
            {
                ErrorHandlers.PrintException(ex);
                return null;
            }
        }

        /// <summary>
        /// Read an expression, evaluate it, and print the results.
        /// </summary>
        /// <returns>If end of file, InputPort.Eof, otherwise expr.</returns>
        public Obj ReadEvalPrint()
        {
            try
            {
                return this.UnsafeReadEvalPrint();
            }
            catch (Exception ex)
            {
                ErrorHandlers.PrintException(ex);
                return Undefined.Instance;
            }
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
                this.CurrentOutputPort.Write(Prompt);
                this.CurrentOutputPort.Flush();
                if (InputPort.IsEof(expr = this.CurrentInputPort.Read()))
                {
                    return new CompletedAsyncResult<string>(InputPort.Eof);
                }

                return this.UnsafeBeginEval(
                    expr,
                    ar =>
                        {
                            object val = this.UnsafeEndEval(ar);
                            if (val != Undefined.Instance)
                            {
                                string output = Printer.AsString(val, false);
                                this.CurrentOutputPort.WriteLine(output);
                            }
                        },
                    null);
            }
            catch (Exception ex)
            {
                ErrorHandlers.PrintException(ex);
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
        public void ReadEvalPrintLoop()
        {
            while (true)
            {
                Obj expr = this.ReadEvalPrint();
                if (ReferenceEquals(expr, InputPort.Eof))
                {
                    return;
                }
            }
        }

        /// <summary>
        /// Load a file.  
        /// Open the file and read it.
        /// Evaluate whatever it contains.
        /// This may be one or more expressions.
        /// If any of them are asynchronous, then the evaluation is NOT blocked, but continues on.
        /// </summary>
        /// <param name="fileName">The filename.</param>
        public void LoadFile(Obj fileName)
        {
            string name = "-- bad file name";
            try
            {
                name = Printer.AsString(fileName, false);
                using (var fs = new FileStream(name, FileMode.Open, FileAccess.Read))
                {
                    this.Load(new InputPort(new StreamReader(fs), this));
                }
            }
            catch (IOException)
            {
                ErrorHandlers.IoError("Can't load " + name);
            }
        }

        /// <summary>
        /// Read from a string and evaluate.
        /// This is done for the side effects, not the value.
        /// This may read several expressions.
        /// </summary>
        /// <param name="str">The string to read and evaluate.</param>
        public void Load(string str)
        {
            try
            {
                using (StringReader reader = new StringReader(str))
                {
                    this.Load(new InputPort(reader, this));
                }
            }
            catch (Exception ex)
            {
                ErrorHandlers.PrintException(ex);
            }
        }

        /// <summary>
        /// Read a single expression from the input port.
        /// </summary>
        /// <param name="inp">The input port to read from.</param>
        /// <returns>The object that was read.</returns>
        public Obj Read(InputPort inp)
        {
            try
            {
                return this.UnsafeRead(inp);
            }
            catch (Exception ex)
            {
                ErrorHandlers.PrintException(ex);
                return Undefined.Instance;
            }
        }

        /// <summary>
        /// Read an expression from the string and parse it into a Scheme object.
        /// </summary>
        /// <param name="str">The string to read.</param>
        /// <returns>The object that was read</returns>
        public Obj Read(string str)
        {
            try
            {
                return this.UnsafeRead(str);
            }
            catch (Exception ex)
            {
                ErrorHandlers.PrintException(ex);
                return Undefined.Instance;
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
                return this.UnsafeEval(this.UnsafeRead(inp));
            }
            catch (Exception ex)
            {
                ErrorHandlers.PrintException(ex);
                return Undefined.Instance;
            }
        }

        /// <summary>
        /// Evaluate a string and return the result.
        /// </summary>
        /// <param name="str">The program to evaluate.</param>
        /// <returns>The evaluation result.</returns>
        public Obj ReadEval(string str)
        {
            try
            {
                return this.UnsafeEval(this.UnsafeRead(str));
            }
            catch (Exception ex)
            {
                ErrorHandlers.PrintException(ex);
                return Undefined.Instance;
            }
        }

        /// <summary>
        /// Read from the given port and evaluate the expression, and return the result
        /// as a string.
        /// </summary>
        /// <param name="inp">The input port to read from.</param>
        /// <returns>The result of the evaluation.</returns>
        public string ReadEvalPrint(InputPort inp)
        {
            try
            {
                return Printer.AsString(this.UnsafeEval(this.UnsafeRead(inp)));
            }
            catch (Exception ex)
            {
                ErrorHandlers.PrintException(ex);
                return String.Empty;
            }
        }

        /// <summary>
        /// Read and evaluate a string and return the result as a string.
        /// </summary>
        /// <param name="str">The program to evaluate.</param>
        /// <returns>The evaluation result.</returns>
        public string ReadEvalPrint(string str)
        {
            try
            {
                return Printer.AsString(this.UnsafeEval(this.UnsafeRead(str)));
            }
            catch (Exception ex)
            {
                ErrorHandlers.PrintException(ex);
                return String.Empty;
            }
        }

        /// <summary>
        /// Create a printable representation of the object.
        /// </summary>
        /// <param name="obj">The object to print.</param>
        /// <returns>The string representing the object.</returns>
        public string Print(Obj obj)
        {
            try
            {
                return Printer.AsString(obj);
            }
            catch (Exception ex)
            {
                ErrorHandlers.PrintException(ex);
                return string.Empty;
            }
        }

        #endregion

        #region Public Methods (Not Exposed)
        /// <summary>
        /// Evaluate an expression in an environment.
        /// Do it by executing a set of steps.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment in which to evaluate it.</param>
        /// <returns>The result of the evaluation.</returns>
        public Obj Eval(Obj expr, Environment env)
        {
            return this.EvalSteps(EvaluateExpression.Call(expr, env, this.halted));
        }

        /// <summary>
        /// Perform steps until evaluation is complete or suspended.
        /// After suspension, return to this entry point.
        /// </summary>
        /// <param name="step">The step to perform first.</param>
        /// <returns>The evaluation result, or suspended evaluator.</returns>
        public Obj EvalSteps(Evaluator step)
        {
            while (true)
            {
                if (step == null)
                {
                    return ErrorHandlers.InternalError("PC bad value");
                }

                if (step.IsSuspended)
                {
                    // See if evaluator wants to handle
                    Evaluator s = SearchForHandler(step);
                    if (s == null)
                    {
                        // nope
                        return step;
                    }

                    // this evaluator wants to -- run it now
                    step = s;
                }

                if (step.IsHalted)
                {
                    if (this.asyncResult != null)
                    {
                        this.asyncResult.SetAsCompleted(this.halted.ReturnedExpr, false);
                    }

                    return step.ReturnedExpr;
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
        public Obj Load(InputPort inp)
        {
            while (true)
            {
                Obj input;
                if (InputPort.IsEof(input = inp.Read()))
                {
                    inp.Close();
                    return Undefined.Instance;
                }

                this.Eval(input);
            }
        }

        /// <summary>
        /// Increment the given counter.
        /// Skip if if counting is turned off.
        /// </summary>
        /// <param name="counterId">The counter to increment.</param>
        public void IncrementCounter(int counterId)
        {
            if (this.Count)
            {
                this.CurrentCounters.Increment(counterId);
            }
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Sets tracing on or off.
        /// </summary>
        /// <param name="caller">The calling evaluator.</param>
        /// <param name="flag">The new trace state.</param>
        /// <returns>Undefined object.</returns>
        private static Obj SetTraceFlag(Evaluator caller, bool flag)
        {
            caller.Interp.Trace = flag;
            return Undefined.Instance;
        }

        /// <summary>
        /// Sets counting on or off.
        /// </summary>
        /// <param name="caller">The calling evaluator.</param>
        /// <param name="flag">The new count state.</param>
        /// <returns>Undefined object.</returns>
        private static Obj SetCountFlag(Evaluator caller, bool flag)
        {
            caller.Interp.Count = flag;
            return Undefined.Instance;
        }

        /// <summary>
        /// When a step is suspended, check with each caller up the chain, seeing if any
        ///   one of them want to resume.
        /// </summary>
        /// <param name="step">The suspended step.</param>
        /// <returns>The evaluator that wants to handle suspension, orherwise null</returns>
        private static Evaluator SearchForHandler(Evaluator step)
        {
            while (!step.IsHalted)
            {
                if (step.CatchSuspended)
                {
                    return step;
                }

                step = step.Caller;
            }

            return null;
        }

        /// <summary>
        /// Display a stack backtrace.
        /// </summary>
        /// <param name="caller">The caller.</param>
        /// <returns>Undefined result.</returns>
        private static Obj Backtrace(Evaluator caller)
        {
            caller.Interp.CurrentOutputPort.WriteLine(caller.StackBacktrace());
            return Undefined.Instance;
        }
        #endregion

        #region Unsafe Private Methods
        /// <summary>
        /// Read a single expression from the input port.
        /// </summary>
        /// <param name="inp">The input port to read from.</param>
        /// <returns>The object that was read.</returns>
        private Obj UnsafeRead(InputPort inp)
        {
            return inp.Read();
        }

        /// <summary>
        /// Read a single expression given as a string.
        /// </summary>
        /// <param name="str">The string to read from.</param>
        /// <returns>The object that was read.</returns>
        private Obj UnsafeRead(string str)
        {
            using (StringReader reader = new StringReader(str))
            {
                return this.UnsafeRead(new InputPort(reader, this));
            }
        }

        /// <summary>
        /// Evaluate an expression (expressed as a list) in the global environment.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The result of the evaluation.</returns>
        private Obj UnsafeEval(Obj expr)
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
        private IAsyncResult UnsafeBeginEval(Obj expr, AsyncCallback cb, object state)
        {
            this.asyncResult = new AsyncResult<object>(cb, state);
            Obj res = this.Eval(expr, this.GlobalEnvironment);
            if (res is Evaluator && ((Evaluator)res).IsSuspended)
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
        private object UnsafeEndEval(IAsyncResult ar)
        {
            return ((AsyncResult<object>)ar).EndInvoke();
        }

        /// <summary>
        /// Read an expression, evaluate it, and print the results.
        /// </summary>
        /// <returns>If end of file, InputPort.Eof, otherwise expr.</returns>
        private Obj UnsafeReadEvalPrint()
        {
            Obj expr;
            this.CurrentOutputPort.Write(Prompt);
            this.CurrentOutputPort.Flush();
            if (InputPort.IsEof(expr = this.CurrentInputPort.Read()))
            {
                return InputPort.Eof;
            }

            Obj val = this.UnsafeEval(expr);
            if (val != Undefined.Instance)
            {
                string output = Printer.AsString(val, false);
                if (output.Length > 0)
                {
                    this.CurrentOutputPort.WriteLine(output);
                }
            }

            return val;
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Write trace info if trace enabled.
        /// </summary>
        /// <param name="step">The step to trace.</param>
        private void TraceStep(Evaluator step)
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

            step.Interp.CurrentOutputPort.WriteLine(String.Format("{0}: {1}", info, step.Expr));
        }
        #endregion
    }
}