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
    public sealed class Interpreter
    {
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
        
        /// <summary>
        /// Initializes a new instance of the Interpreter class.
        /// Create an interpreter and install the primitives into the global environment.
        /// Then read a list of files.
        /// </summary>
        /// <param name="loadStandardMacros">Load standard macros and other primitives.</param>
        /// <param name="primEnvironment">Environment containing the primitives (can be null).</param>
        /// <param name="files">The files to read.</param>
        public Interpreter(bool loadStandardMacros, Environment primEnvironment, IEnumerable<string> files)
        {
            this.halted = new EvaluatorBase("halted");
            this.Trace = false;
            this.Count = false;
            this.Input = new InputPort(Console.In);
            this.Output = new OutputPort(Console.Out);
            if (primEnvironment == null)
            {
                primEnvironment = new Environment()
                    .InstallPrimitives();
            }

            this.Counters = new Counter();
            this.GlobalEnvironment = new Environment(this, primEnvironment);

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
        /// Set up the most common way -- with standard macros and no files.
        /// </summary>
        /// <param name="name">The evaluator name.</param>
        public Interpreter(string name)
            : this(true, null, null)
        {
            this.halted = new EvaluatorBase(name);
        }

        /// <summary>
        /// Initializes a new instance of the Interpreter class.
        /// Read a set of files initially.
        /// </summary>
        /// <param name="files">The files to read.</param>
        public Interpreter(IEnumerable<string> files)
            : this(true, null, files)
        {
        }

        /// <summary>
        /// Gets the counters collection.
        /// </summary>
        public Counter Counters { get; private set; }

        /// <summary>
        /// Gets the input port.
        /// </summary>
        internal InputPort Input { get; private set; }

        /// <summary>
        /// Gets the output port.
        /// </summary>
        internal OutputPort Output { get; private set; }

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

        /// <summary>
        /// Define the counter primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(Environment env)
        {
            env
                .DefinePrimitive("trace-on", (caller, args) => caller.Env.Interp.Trace = true, 0)
                .DefinePrimitive("trace-off", (caller, args) => caller.Env.Interp.Trace = false, 0)
                .DefinePrimitive("counters-on", (caller, args) => caller.Env.Interp.Count = true, 0)
                .DefinePrimitive("counters-off", (caller, args) => caller.Env.Interp.Count = false, 0)
                .DefinePrimitive(
                    "backtrace",
                    (caller, args) =>
                        {
                            Console.Out.WriteLine(caller.StackBacktrace());
                            return null;
                        },
                    0);
        }

        /// <summary>
        /// Evaluate an expression (expressed as a list) in the global environment.
        /// </summary>
        /// <param name="x">The expression to evaluate.</param>
        /// <returns>The result of the evaluation.</returns>
        public object Eval(object x)
        {
            return this.Eval(x, this.GlobalEnvironment);
        }

        /// <summary>
        /// Begin an asynchronous evaluation
        /// </summary>
        /// <param name="x">The expression to evaluate.</param>
        /// <param name="cb">Call this when evaluation is complete.</param>
        /// <param name="state">Pass this through for the callback function.</param>
        /// <returns>Async result, used to monitor progress.</returns>
        public IAsyncResult BeginEval(object x, AsyncCallback cb, object state)
        {
            this.asyncResult = new AsyncResult<object>(cb, state);
            object res = this.Eval(x, this.GlobalEnvironment);
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
        /// Evaluate an expression in an environment.
        /// Do it by executing a set of steps.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment in which to evaluate it.</param>
        /// <returns>The result of the evaluation.</returns>
        public object Eval(object expr, Environment env)
        {
            return this.EvalStep(EvaluateExpression.Call(this.halted, expr, env));
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
        public object EvalStep(Stepper step)
        {
            Stepper lastStep = null;
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

                if (this.Trace && lastStep != step)
                {
                    string info = step.TraceInfo();
                    if (info != null)
                    {
                        Console.Out.WriteLine("{0}: {1}", info, step.Expr);
                    }
                }

                if (step == null)
                {
                    return ErrorHandlers.EvalError("PC bad value");
                }

                step.IncrementCounter(counter);
                lastStep = step;
                step = step.Pc();
            }
        }

        /// <summary>
        /// Load a file.  
        /// Open the file and read it.
        /// Evaluate whatever it contains.
        /// </summary>
        /// <param name="fileName">The filename.</param>
        /// <returns>The result of evaluating the file contents.</returns>
        public object LoadFile(object fileName)
        {
            string name = SchemeString.AsString(fileName, false);
            try
            {
                return this.Load(
                    new InputPort(new FileStream(name, FileMode.Open, FileAccess.Read)));
            }
            catch (IOException)
            {
                return ErrorHandlers.Error("Load: can't load " + name);
            }
        }

        /// <summary>
        /// Read from the input port and evaluate whatever is there.
        /// </summary>
        /// <param name="inp">The input port.</param>
        /// <returns>True always.</returns>
        public object Load(InputPort inp)
        {
            while (true)
            {
                object x;
                if (InputPort.IsEOF(x = inp.Read()))
                {
                    inp.Close();
                    return SchemeBoolean.True;
                }

                object val = this.Eval(x);
            }
        }

        /// <summary>
        /// Read from an input port, evaluate in the global environment, and print the result.
        /// Catch and discard exceptions.
        /// </summary>
        /// <returns>The interpreter.</returns>
        public Interpreter ReadEvalWriteLoop()
        {
            while (true)
            {
                try
                {
                    object x;
                    this.Output.Print("> ");
                    this.Output.Flush();
                    if (InputPort.IsEOF(x = this.Input.Read()))
                    {
                        return this;
                    }

#if ASYNC
                    this.BeginEval(
                        x,
                        ar =>
                            {
                                object val = this.EndEval(ar);
                                OutputPort.Write(val, this.Output, true);
                                this.Output.Println();
                                this.Output.Flush();
                            },
                        null);
#else
                    object val = this.Eval(x);
                    OutputPort.Write(val, this.Output, true);
                    this.Output.Println();
                    this.Output.Flush();
#endif
                }
                catch (Exception ex)
                {
                    Console.WriteLine("Caught exception {0}", ex.Message);
                }
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
                this.Counters.Increment(counterId);
            }
        }

        /// <summary>
        /// Read from a string and evaluate.
        /// </summary>
        /// <param name="str">The string to read and evaluate.</param>
        /// <returns>The result of the evaluation</returns>
        private object LoadString(string str)
        {
            using (StringReader reader = new StringReader(str))
            {
                return this.Load(new InputPort(reader));
            }
        }
    }
}