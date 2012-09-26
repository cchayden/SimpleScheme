// <copyright file="Interpreter.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;
    using System.IO;

    /// <summary>
    /// The scheme interpreter instance.
    /// Each one of these is a complete interpreter, independent of others.
    /// </summary>
    public sealed class Interpreter : IInterpreter
    {
        #region Fields
        /// <summary>
        /// The initial step.  When complete, evaluation is done.
        /// </summary>
        private readonly Evaluator halted;

        /// <summary>
        /// One of the parallel steps is ended
        /// </summary>
        private readonly Evaluator ended;

        /// <summary>
        /// The async result used in case the interpreter is called asynchronously.
        /// </summary>
        private AsyncResult<SchemeObject> asyncResult;
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
        private Interpreter(
            bool loadStandardMacros, 
            PrimitiveEnvironment primEnvironment, 
            IEnumerable<string> files, 
            TextReader reader, 
            TextWriter writer)
        {
            this.Trace = false;
            this.Count = false;
            this.Transcript = new TranscriptLogger(this);
            this.CurrentInputPort = InputPort.New(reader ?? Console.In, this);
            this.CurrentOutputPort = OutputPort.New(writer ?? Console.Out, this);
            this.PrimEnvironment = primEnvironment ?? new PrimitiveEnvironment();

            this.CurrentCounters = new Counter();
            this.GlobalEnvironment = new Environment(this, this.PrimEnvironment);
            this.halted = new HaltedEvaluator(this.GlobalEnvironment);
            this.ended = new EndedEvaluator(this.GlobalEnvironment);
            var echo = (writer != null) ? this.CurrentOutputPort : null;
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
                        this.LoadFile((Symbol)file, echo);
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
        private Interpreter(
            bool loadStandardMacros, 
            IPrimitiveEnvironment primEnvironment, 
            IEnumerable<string> files, 
            TextReader reader, 
            TextWriter writer) :
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
        /// Gets the ended evaluator.
        /// </summary>
        public Evaluator Ended
        {
            get { return this.ended; }
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
        /// <param name="echo">If true, echo input and output while reading files.</param>
        /// <returns>A scheme interpreter.</returns>
        public static IInterpreter New(IEnumerable<string> files, bool echo)
        {
            return new Interpreter(true, null, files, null, echo ? Console.Out : null);
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
        public static IInterpreter New(
            bool loadStandardMacros, 
            IPrimitiveEnvironment primEnvironment, 
            IEnumerable<string> files, 
            TextReader reader, 
            TextWriter writer)
        {
            return new Interpreter(loadStandardMacros, primEnvironment, files, reader, writer);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Set the asynchronous completion value, if we were
        ///   doing an async call.
        /// </summary>
        /// <param name="returnedExpr">The value to return as the asynchronous result.</param>
        public void SetComplete(SchemeObject returnedExpr)
        {
            if (this.asyncResult != null)
            {
                this.asyncResult.SetAsCompleted(returnedExpr, false);
            }
        }

        /// <summary>
        /// Evaluate an expression (expressed as a list) in the global environment.
        /// Catch any exceptions that may happen.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The result of the evaluation.</returns>
        public SchemeObject Eval(SchemeObject expr)
        {
            this.asyncResult = null;
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
        /// Evaluate an expression (expressed as a string) in the global environment.
        /// </summary>
        /// <param name="str">The expression to evaluate.</param>
        /// <returns>The result of the evaluation.</returns>
        public SchemeObject Eval(string str)
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
        /// Begin an asynchronous evaluation.
        /// Catch any exceptions that might happen.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="cb">Call this when evaluation is complete.</param>
        /// <param name="state">Pass this through for the callback function.</param>
        /// <returns>Async result, used to monitor progress.</returns>
        public IAsyncResult BeginEval(SchemeObject expr, AsyncCallback cb, object state)
        {
            this.asyncResult = null;
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
        public SchemeObject EndEval(IAsyncResult ar)
        {
            try
            {
                return UnsafeEndEval(ar);
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
        public SchemeObject ReadEvalPrint()
        {
            this.asyncResult = null;
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
            this.asyncResult = null;
            try
            {
                SchemeObject expr;
                if ((expr = this.CurrentInputPort.Read()) is Eof)
                {
                    return new CompletedAsyncResult<Eof>(Eof.Instance);
                }

                return this.UnsafeBeginEval(
                    expr,
                    ar =>
                        {
                            SchemeObject val = UnsafeEndEval(ar);
                            if (val != Undefined.Instance)
                            {
                                string output = val.ToString(false);
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
                SchemeObject expr = this.ReadEvalPrint();
                if (ReferenceEquals(expr, Eof.Instance))
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
        /// <param name="outp">If not null, input and results are written here.</param>
        public void LoadFile(SchemeObject fileName, OutputPort outp)
        {
            string name = string.Empty;
            try
            {
                name = fileName.ToString();
                using (var fs = new FileStream(name, FileMode.Open, FileAccess.Read))
                {
                    this.Load(InputPort.New(new StreamReader(fs), this), outp);
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
                using (var reader = new StringReader(str))
                {
                    this.Load(InputPort.New(reader, this), null);
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
        public SchemeObject Read(InputPort inp)
        {
            try
            {
                return UnsafeRead(inp);
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
        public SchemeObject Read(string str)
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
        public SchemeObject ReadEval(InputPort inp)
        {
            try
            {
                return this.UnsafeEval(UnsafeRead(inp));
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
                return this.UnsafeEval(UnsafeRead(inp)).ToString(true);
            }
            catch (Exception ex)
            {
                ErrorHandlers.PrintException(ex);
                return string.Empty;
            }
        }

        /// <summary>
        /// Read and evaluate a string and return the result as a string.
        /// </summary>
        /// <param name="str">The program to evaluate.</param>
        /// <returns>The evaluation result.</returns>
        public string ReadEvalPrint(string str)
        {
            this.asyncResult = null;
            try
            {
                return this.UnsafeEval(this.UnsafeRead(str)).ToString(true);
            }
            catch (Exception ex)
            {
                ErrorHandlers.PrintException(ex);
                return string.Empty;
            }
        }

        /// <summary>
        /// Create a printable representation of the object.
        /// </summary>
        /// <param name="obj">The object to print.</param>
        /// <returns>The string representing the object.</returns>
        public string Print(SchemeObject obj)
        {
            try
            {
                return obj.ToString(true);
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
        internal SchemeObject Eval(SchemeObject expr, Environment env)
        {
            return this.EvalSteps(EvaluateExpression.Call(expr, env, this.halted));
        }

        /// <summary>
        /// Perform steps until evaluation is complete or suspended.
        /// </summary>
        /// <param name="step">The step to perform first.</param>
        /// <returns>The evaluation result, or suspended evaluator.</returns>
        internal SchemeObject EvalSteps(Evaluator step)
        {
            while (true)
            {
#if Check
                if (step == null)
                {
                    return ErrorHandlers.InternalError("PC bad value");
                }
#endif

                Evaluator nextStep = step.NextStep();
                if (nextStep == null)
                {
                    return step.ReturnedExpr;
                }

                // run the step and capture the next step
                step = nextStep.RunStep();
            }
        }

        /// <summary>
        /// Read from the input port and evaluate whatever is there.
        /// Done for the side effect, not the result.
        /// Since the result is never tested, asynchronous suspensions do not prevent the rest
        /// of the file from being loaded.
        /// </summary>
        /// <param name="inp">The input port.</param>
        /// <param name="outp">If not null, input and results are written here.</param>
        /// <returns>Undefined instance.</returns>
        internal SchemeObject Load(InputPort inp, OutputPort outp)
        {
            while (true)
            {
                SchemeObject input;
                if ((input = inp.Read()) is Eof)
                {
                    inp.Close();
                    return Undefined.Instance;
                }

                if (outp != null)
                {
                    outp.WriteLine("> " + input);
                }

                var res = this.Eval(input);
                if (outp != null)
                {
                    outp.WriteLine(res.ToString());
                }
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
                this.CurrentCounters.Increment(counterId);
            }
        }
        #endregion

        #region Unsafe Private Methods
        /// <summary>
        /// Read a single expression from the input port.
        /// </summary>
        /// <param name="inp">The input port to read from.</param>
        /// <returns>The object that was read.</returns>
        private static SchemeObject UnsafeRead(InputPort inp)
        {
            return inp.Read();
        }

        /// <summary>
        /// End asynchronous evaluation and get the result.
        /// </summary>
        /// <param name="ar">Async result from callback.</param>
        /// <returns>The expression value.</returns>
        private static SchemeObject UnsafeEndEval(IAsyncResult ar)
        {
            var res = ((AsyncResult<SchemeObject>)ar).EndInvoke();
            return res;
        }

        /// <summary>
        /// Read a single expression given as a string.
        /// </summary>
        /// <param name="str">The string to read from.</param>
        /// <returns>The object that was read.</returns>
        private SchemeObject UnsafeRead(string str)
        {
            using (var reader = new StringReader(str))
            {
                return UnsafeRead(InputPort.New(reader, this));
            }
        }

        /// <summary>
        /// Evaluate an expression (expressed as a list) in the global environment.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The result of the evaluation.</returns>
        private SchemeObject UnsafeEval(SchemeObject expr)
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
        private IAsyncResult UnsafeBeginEval(SchemeObject expr, AsyncCallback cb, object state)
        {
            this.asyncResult = new AsyncResult<SchemeObject>(cb, state);
            SchemeObject res = this.Eval(expr, this.GlobalEnvironment);
            if ((res is ClrObject) && ((ClrObject)res).Value is SuspendedEvaluator)
            {
                return this.asyncResult;
            }

            if (!this.asyncResult.IsCompleted)
            {
                this.asyncResult.SetAsCompleted(EvaluatorOrObject.EnsureSchemeObject(res), true);
            }

            return this.asyncResult;
        }

        /// <summary>
        /// Read an expression, evaluate it, and print the results.
        /// The reader does not read the trailing newline: it returns as soon as it sees that the
        ///   expression has been completed.  This means that the line number does not get incremented by the
        ///   first Console read, and is incremented the first thing on subsequent reads.  So the line number does not
        ///   appear to increment on the very first expression.
        /// </summary>
        /// <returns>If end of file, InputPort.Eof, otherwise expr.</returns>
        private SchemeObject UnsafeReadEvalPrint()
        {
            SchemeObject expr;
            if ((expr = this.CurrentInputPort.Read()) is Eof)
            {
                return Eof.Instance;
            }

            SchemeObject val = this.UnsafeEval(expr);
            if (val != Undefined.Instance)
            {
                string output = val.ToString(false);
                if (output.Length > 0)
                {
                    this.CurrentOutputPort.WriteLine(output);
                }
            }

            return val;
        }
        #endregion
    }
}