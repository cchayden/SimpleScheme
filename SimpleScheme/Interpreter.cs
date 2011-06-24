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
    public sealed class Interpreter
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
        public Interpreter(bool loadStandardMacros, Environment primEnvironment, IEnumerable<string> files, TextReader reader, TextWriter writer)
        {
            this.Trace = false;
            this.Count = false;
            this.Input = InputPort.New(reader ?? Console.In);
            this.Output = OutputPort.New(writer ?? Console.Out);
            if (primEnvironment == null)
            {
                primEnvironment = Environment.NewPrimitive();
                Environment.InstallPrimitives(primEnvironment);
            }

            this.Counters = new Counter();
            this.GlobalEnvironment = Environment.NewGlobal(this, primEnvironment);
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

        /// <summary>
        /// Initializes a new instance of the Interpreter class.
        /// Create an interpreter and install the primitives into the global environment.
        /// Then read a list of files.
        /// Finally, accept console input.
        /// </summary>
        /// <param name="loadStandardMacros">Load standard macros and other primitives.</param>
        /// <param name="primEnvironment">Environment containing the primitives (can be null).</param>
        /// <param name="files">The files to read.</param>
        public Interpreter(bool loadStandardMacros, Environment primEnvironment, IEnumerable<string> files)
            : this(loadStandardMacros, primEnvironment, files, null, null)
        {
        }

        /// <summary>
        /// Initializes a new instance of the Interpreter class.
        /// Read a set of files initially.
        /// Use the supplied reader and writer.
        /// To skip interactive reading, use TextReader.Null.
        /// Or do not call ReadEvalPrint or ReadEvalPrintLoop.
        /// </summary>
        /// <param name="files">The files to read.</param>
        /// <param name="reader">The input reader.</param>
        /// <param name="writer">The output writer.</param>
        public Interpreter(IEnumerable<string> files, TextReader reader, TextWriter writer)
            : this(true, null, files, reader, writer)
        {
        }

        /// <summary>
        /// Initializes a new instance of the Interpreter class.
        /// Read a set of files initially, then do interactive input.
        /// </summary>
        /// <param name="files">The files to read.</param>
        public Interpreter(IEnumerable<string> files)
            : this(true, null, files, null, null)
        {
        }

        /// <summary>
        /// Initializes a new instance of the Interpreter class.
        /// Set up for tests, with standard macros and no files.
        /// </summary>
        public Interpreter()
            : this(true, null, null)
        {
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the input port.
        /// </summary>
        public InputPort Input { get; private set; }

        /// <summary>
        /// Gets the output port.
        /// </summary>
        public OutputPort Output { get; private set; }

        /// <summary>
        /// Gets the counters collection.
        /// </summary>
        internal Counter Counters { get; private set; }

        /// <summary>
        /// Gets or sets a value indicating whether to trace.
        /// </summary>
        private bool Trace { get; set; }

        /// <summary>
        /// Gets or sets a value indicating whether to count.
        /// </summary>
        private bool Count { get; set; }

        /// <summary>
        /// Gets or setsthe global environment for the interpreter.
        /// </summary>
        private Environment GlobalEnvironment { get; set; }
        #endregion

        #region Public Methods
        /// <summary>
        /// Evaluate an expression (expressed as a list) in the global environment.
        /// </summary>
        /// <param name="x">The expression to evaluate.</param>
        /// <returns>The result of the evaluation.</returns>
        public Obj Eval(Obj x)
        {
            return this.Eval(x, this.GlobalEnvironment);
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
        /// <returns>If end of file, InputPort.Eof, otherwise expr.</returns>
        public Obj ReadEvalPrintAsync()
        {
            try
            {
                Obj expr;
                this.Output.Print("> ");
                this.Output.Flush();
                if (InputPort.IsEof(expr = this.Input.Read()))
                {
                    return InputPort.Eof;
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
                return Undefined.Instance;
            }
        }

        /// <summary>
        /// Read from an input port, evaluate in the global environment, and print the result.
        /// If value is undefined, do not print anything.
        /// If the result is suspended, then don't print anything either.
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
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the counter primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        internal static void DefinePrimitives(Environment env)
        {
            env
                //// (trace-on)
                .DefinePrimitive("trace-on", (args, caller) => caller.Env.Interp.Trace = true, 0)
                //// (trace-off)
                .DefinePrimitive("trace-off", (args, caller) => caller.Env.Interp.Trace = false, 0)
                //// (counters-on)
                .DefinePrimitive("counters-on", (args, caller) => caller.Env.Interp.Count = true, 0)
                //// (counters-off)
                .DefinePrimitive("counters-off", (args, caller) => caller.Env.Interp.Count = false, 0)
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

        // TODO instead of calling Name in the step, call TraceStep.
        // TODO TraceStep should show name and any relevant operands
        // TODO evaluate-proc should show the proc name
        // TODO It should do it only once per stepper

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
                    return ErrorHandlers.EvalError("PC bad value");
                }

                this.TraceStep(step);
                step.IncrementCounter(counter);
                step = step.RunStep();
            }
        }

        /// <summary>
        /// Load a file.  
        /// Open the file and read it.
        /// Evaluate whatever it contains.
        /// </summary>
        /// <param name="fileName">The filename.</param>
        /// <returns>Undefined value.</returns>
        internal Obj LoadFile(object fileName)
        {
            string name = SchemeString.AsString(fileName, false);
            try
            {
                return this.Load(
                    InputPort.New(new FileStream(name, FileMode.Open, FileAccess.Read)));
            }
            catch (IOException)
            {
                return ErrorHandlers.Error("Can't load " + name);
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
                if (InputPort.IsEof(input = inp.Read()))
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
                if (InputPort.IsEof(expr = this.Input.Read()))
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
        /// Display a stack backtrace.
        /// </summary>
        /// <param name="caller">The caller.</param>
        /// <returns>Undefined result.</returns>
        private static Obj Backtrace(Stepper caller)
        {
            Console.Out.WriteLine(caller.StackBacktrace());
            return Undefined.Instance;
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Read from a string and evaluate.
        /// This is done for the side effects, not the value.
        /// </summary>
        /// <param name="str">The string to read and evaluate.</param>
        private void LoadString(string str)
        {
            using (StringReader reader = new StringReader(str))
            {
                this.Load(InputPort.New(reader));
            }
        }

        /// <summary>
        /// Write trace info if trace enabled.
        /// </summary>
        /// <param name="step">The step to trace.</param>
        private void TraceStep(Stepper step)
        {
            if (this.Trace)
            {
                string info = step.TraceInfo();
                if (info != null)
                {
                    Console.Out.WriteLine("{0}: {1}", info, step.Expr);
                }
            }
        }
        #endregion
    }
}