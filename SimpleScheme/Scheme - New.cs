// <copyright file="Scheme.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
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
    public sealed class Scheme : SchemeUtils
    {
        /// <summary>
        /// The input port for the interpreter.
        /// </summary>
        private readonly InputPort input = new InputPort(Console.In);

        /// <summary>
        /// The output port for the interpreter.
        /// </summary>
        private readonly PrintWriter output = new PrintWriter(Console.Out);

        /// <summary>
        /// The interpreter global environment.
        /// </summary>
        private readonly Environment globalEnvironment = new Environment();

        /// <summary>
        /// Initializes a new instance of the Scheme class.
        /// Create an interpreter and install the primitives into the global environment.
        /// Then read a list of files.
        /// </summary>
        /// <param name="files">The files to read.</param>
        public Scheme(IEnumerable<string> files)
        {
            Primitive.InstallPrimitives(this.GlobalEnvironment);
            try
            {
                // TODO isn't there overlap between the installed primitives and the ones read in?
                this.Load(SchemePrimitives.Code);
                foreach (string file in files)
                {
                    this.Load(file);
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine("Caught exception {0}", ex.Message);
            }
        }

        /// <summary>
        /// Gets the input port.
        /// </summary>
        internal InputPort Input
        {
            get { return this.input; }
        }

        /// <summary>
        /// Gets the output port.
        /// </summary>
        internal PrintWriter Output
        {
            get { return this.output; }
        }

        /// <summary>
        /// Gets the global environment for the interpreter.
        /// </summary>
        internal Environment GlobalEnvironment
        {
            get { return this.globalEnvironment; }
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
        /// Evaluate an expression in the given environment.
        /// Besides all the special forms, the main action is to:
        ///   return strings and constants, which evaluate to themselves
        ///   treat the first arg as a procedure, evaluate the rest of the expression args, 
        ///   and apply the procedure to the evaluated results.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to evaluate it in.</param>
        /// <returns>The result of the evaluation.</returns>
        public object Eval(object expr, Environment env)
        {
            while (true)
            {
                object fn;
                object args;
                string fnString;
                object name;
                object value;
                object result;
                if (expr is string)
                {
                    // Evaluate a string by looking it up in the environment.
                    // It should correspond to a varialbe name, for which there 
                    //    is a corresponding value.
                    return env.Lookup((string)expr);
                }

                if (!(expr is Pair))
                {
                    // If we are evaluating something that is not a pair, 
                    //    it must be a constant.
                    // Return the integer, real, boolean, or vector.
                    return expr;
                }

                // We are evaluating a pair.
                // Split out the first item for special treatment.
                fn = First(expr);
                args = Rest(expr);

                // Look for one of the special forms. 
                fnString = fn as string;
                if (fnString == "quote")
                {

                    //   the expression unevaluated.
                    return First(args);
                }
                if (fnString == "begin")
                {
                    // Evaluate begin by evaluating all the items in order, 
                    //   and returning the last.
// >>>>>>>>>
                    expr = EvalSequence(args, env);
                    continue;
                }

                if (fnString == "define")
                {
                    // Define is a shortcut for lambda.
                    // Evaluate by splicing lambda on the front and evaluating that.
                    object body;
                    if (First(args) is Pair)
                    {
                        name = First(First(args));
                        body = Cons("lambda", Cons(Rest(First(args)), Rest(args)));
                    }
                    else
                    {
                        name = First(args);
                        body = Second(args);
                    }
//=======
                    value = this.Eval(body, env);
                    return env.Define(name, value);
//=======
                }

                if (fnString == "set!")
                {
                    // Evaluate a set! expression by evaluating the second, 
                    //   then setting the first to it.
//========
                    value = this.Eval(Second(args), env);
                    return env.Set(First(args), value);
//=======
                }
                if (fnString == "if")
                {
                    // Eval an if expression by evaluating the first clause, 
                    //    and then returning either the second or third.
//=========
                    value = this.Eval(First(args), env);
                    expr = Truth(value) ? Second(args) : Third(args);
                    continue;
//=======
                }

                if (fnString == "cond")
                {
// >>>>>>>>>
                    expr = EvalCond(args, env);
                    continue;
// >>>>>>>>>
                }

                if (fnString == "lambda")
                {
                    // Evaluate a lambda by creating a closure.
                    return new Closure(First(args), Rest(args), env);
                }

                if (fnString == "macro")
                {
                    // Evaluate a macro by creating a macro.
                    return new Macro(First(args), Rest(args), env);
                }

                // If we get here, it wasn't one of the special forms.  
                // So we need to evaluate the first item (the function) in preparation for
                //    doing a procedure call.
// ===================
                fn = this.Eval(fn, env);

                // If the function is a macro, expand it and then continue.
                if (fn is Macro)
                {
                    Macro m = (Macro)fn;
                    expr = m.Expand(this, (Pair)expr, args);
                    continue;
                }

                // If the function is a closure, then create a new environment consisting of
                //   1 the closure param list
                //   2 arguments evaluated in the original environment
                //   3 the closure's environment
                // Then continue evaluating the closure body in this new environment
                if (fn is Closure)
                {
                    // CLOSURE CALL -- capture the environment and continue with the body
                    Closure f = (Closure)fn;
                    expr = f.Body;
// >>>>>>>>>>>>>>>>>>>
                    env = new Environment(f.Parms, this.EvalList(args, env), f.Env);
                    continue;
// >>>>>>>>>>>>>>>>>>>
                }

                // This is a procedure call.
                // In any other case, the function is a primitive or a user-defined function.
                // Evaluate the arguments in the environment, then apply the function 
                //    to the arguments.
// >>>>>>>>>>>>>>>>>>>
                return Procedure.Proc(fn).Apply(this, this.EvalList(args, env));
// >>>>>>>>>>>>>>>>>>>
            }
        }

        /// <summary>
        /// Load a file.  
        /// Open the file and read it.
        /// Evaluate whatever it contains.
        /// </summary>
        /// <param name="fileName">The filename.</param>
        /// <returns>The result of evaluating the file contents.</returns>
        public object Load(object fileName)
        {
            string name = Stringify(fileName, false);
            try
            {
                return this.Load(
                    new InputPort(new FileStream(name, FileMode.Open, FileAccess.Read)));
            }
            catch (IOException)
            {
                return Error("can't load " + name);
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
                    return True;
                }

                this.Eval(x);
            }
        }

        /// <summary>
        /// Read from a string and evaluate.
        /// </summary>
        /// <param name="str">The string to read and evaluate.</param>
        /// <returns>The result of the evaluation</returns>
        public object Load(string str)
        {
            using (StringReader reader = new StringReader(str))
            {
                return this.Load(new InputPort(reader));
            }
        }

        /// <summary>
        /// Read from an input port, evaluate in the global environment, and print the result.
        /// Catch and discard exceptions.
        /// </summary>
        public void ReadEvalWriteLoop()
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
                        return;
                    }

                    Write(this.Eval(x), this.Output, true);
                    this.Output.Println();
                    this.Output.Flush();
                }
                catch (Exception ex)
                {
                    Console.WriteLine("Caught exception {0}", ex.Message);
                }
            }
        }

        /// <summary>
        /// Evaluate a sequence of objects, returning the last.
        /// </summary>
        /// <param name="args">A list of items to evaluate.</param>
        /// <param name="env">The evaluation environment.</param>
        /// <returns>The evaluation of the last</returns>
        private object EvalSequence(object args, Environment env)
        {
                    while (Rest(args) != null)
                    {
// =================
                        this.Eval(First(args), env);
                        args = Rest(args);
                    }

                    return First(args);
            
        } 

        /// <summary>
        /// Evaluate the items in a list, given the environment.
        /// This is done to the args of a procedure call (except for special forms).
        /// This is an iterative, rather than a recursive one.
        /// </summary>
        /// <param name="list">The list of items to evaluate.</param>
        /// <param name="env">The evaluation environment.</param>
        /// <returns>A list of the evaluated items.</returns>
        private Pair EvalList(object list, Environment env)
        {
            if (list == null)
            {
                return null;
            }

            if (!(list is Pair))
            {
                Error("Illegal arg list: " + list);
                return null;
            }

            Pair result = List(null);    // empty cell will be stripped off below
            Pair accum = result;
            while (list is Pair)
            {
// ==================
                object value = this.Eval(First(list), env);
                accum = (Pair)(accum.Rest = List(value));
                list = Rest(list);
            }

            return (Pair)result.Rest;
        }

        private object EvalCond(object args, Environment env)
        {
            // Eval a conditional expression. 
            // Iterate down the list of args.
            // The args are (guard expression) pairs.
            // If they are exhausted, return False.
            // If we find a True guard (or an else)
            //     If the clause has no expression, return the guard.
            //     Otherwise return the expression to be evaluated.
            object result = null;
            object expr;
            while (true)
            {
                if (args == null)
                {
                    expr = False;
                    break;
                }

                object clause = First(args);
                args = Rest(args);
                bool selected;
                if (First(clause) as string == "else")
                {
                    selected = true;
                }
                else
                {
// ===================
                    result = this.Eval(First(clause), env);
                    selected = Truth(result);
                }

                if (!selected)
                {
                    continue;
                }

                if (Rest(clause) == null)
                {
                    expr = List("quote", result);
                    break;
                }

                if (Second(clause) as string == "=>")
                {
                    expr = List(Third(clause), List("quote", result));
                    break;
                }

                expr = Cons("begin", Rest(clause));
                break;
            }
            return expr;
        }
    }
}