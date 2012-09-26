// -----------------------------------------------------------------------
// <copyright file="Debugging.cs" company="">
// Copyright © 2011 by Charles Hayden.
// </copyright>
// -----------------------------------------------------------------------

namespace SimpleScheme
{

    /// <summary>
    /// Debugging primitives
    /// </summary>
    public class Debugging
    {
        #region Define Primitives
        /// <summary>
        /// Define the counter primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(PrimitiveEnvironment env)
        {
            env
                .DefinePrimitive(
                    "trace-on", 
                    new[] { "(trace-on)" },
                    (args, caller) => SetTraceFlag(caller, true), 
                    0)
                .DefinePrimitive(
                    "trace-off", 
                    new[] { "(trace-off)" },
                    (args, caller) => SetTraceFlag(caller, false), 
                    0)
                .DefinePrimitive(
                    "counters-on", 
                    new[] { "(counters-on)" },
                    (args, caller) => SetCountFlag(caller, true), 
                    0)
                .DefinePrimitive(
                    "counters-off", 
                    new[] { "(counters-off)" },
                    (args, caller) => SetCountFlag(caller, false), 
                    0)
                .DefinePrimitive(
                    "backtrace", 
                    new[] { "(backtrace)" },
                    (args, caller) => Backtrace(caller), 
                    0);
                env.DefinePrimitive(
                    "debug", 
                    new[] { "(debug)" },
                    (args, caller) => Debug(caller), 
                    0);
                env.DefinePrimitive(
                    "list-primitives", 
                    new[] { "(list-primitives)" },
                    (args, caller) => caller.Interp.PrimEnvironment.ListPrimitives(), 
                    0);
                env.DefinePrimitive(
                    "describe", 
                    new[] { "(describe <obj>)" },
                    (args, caller) => Describe(List.First(args)), 
                    1, 
                    Primitive.ArgType.Obj);
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Sets tracing on or off.
        /// </summary>
        /// <param name="caller">The calling evaluator.</param>
        /// <param name="flag">The new trace state.</param>
        /// <returns>Undefined object.</returns>
        private static SchemeObject SetTraceFlag(Evaluator caller, bool flag)
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
        private static SchemeObject SetCountFlag(Evaluator caller, bool flag)
        {
            caller.Interp.Count = flag;
            return Undefined.Instance;
        }

        /// <summary>
        /// Display a stack backtrace.
        /// </summary>
        /// <param name="caller">The caller.</param>
        /// <returns>Undefined result.</returns>
        private static SchemeObject Backtrace(Evaluator caller)
        {
            caller.Interp.CurrentOutputPort.WriteLine(caller.StackBacktrace());
            return Undefined.Instance;
        }

        /// <summary>
        /// Display whatever debug information is defined
        /// </summary>
        /// <param name="caller">The calling evaluator.</param>
        /// <returns>Undefined object.</returns>
        private static SchemeObject Debug(Evaluator caller)
        {
            caller.Interp.CurrentOutputPort.WriteLine("debug");
            System.Diagnostics.Debugger.Break();
            return Undefined.Instance;
        }

        /// <summary>
        /// Return a description of the given object.
        /// </summary>
        /// <param name="obj">The object to describe.</param>
        /// <returns>A description in a SchemeString.</returns>
        private static SchemeObject Describe(SchemeObject obj)
        {
            string msg = string.Format("Type: {0}\nDescription: {1}", obj.GetType().Name, obj.Describe());
            return SchemeString.New(msg);
        }
        #endregion
    }
}
