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
    internal sealed class Debugging
    {
        #region Define Primitives
        /// <summary>
        /// Define the counter primitives.
        /// </summary>
        /// <param name="primEnv">The environment to define the primitives into.</param>
        internal static void DefinePrimitives(PrimitiveEnvironment primEnv)
        {
            primEnv
                .DefinePrimitive(
                    "trace-on", 
                    new[] { "(trace-on)" },
                    (args, env, caller) => SetTraceFlag(caller, true), 
                    new ArgsInfo(0))
                .DefinePrimitive(
                    "trace-off", 
                    new[] { "(trace-off)" },
                    (args, env, caller) => SetTraceFlag(caller, false), 
                    new ArgsInfo(0))
                .DefinePrimitive(
                    "counters-on", 
                    new[] { "(counters-on)" },
                    (args, env, caller) => SetCountFlag(caller, true), 
                    new ArgsInfo(0))
                .DefinePrimitive(
                    "counters-off", 
                    new[] { "(counters-off)" },
                    (args, env, caller) => SetCountFlag(caller, false), 
                    new ArgsInfo(0))
                .DefinePrimitive(
                    "backtrace", 
                    new[] { "(backtrace)" },
                    (args, env, caller) => Backtrace(caller), 
                    new ArgsInfo(0))
                .DefinePrimitive(
                    "debug", 
                    new[] { "(debug)" },
                    (args, env, caller) => Debug(caller), 
                    new ArgsInfo(0))
                .DefinePrimitive(
                    "list-primitives", 
                    new[] { "(list-primitives)" },
                    (args, env, caller) => caller.Interp.PrimEnvironment.ListPrimitives(), 
                    new ArgsInfo(0))
                .DefinePrimitive(
                    "describe", 
                    new[] { "(describe <obj>)" },
                    (args, env, caller) => Describe(List.First(args)), 
                    new ArgsInfo(1, ArgType.Obj));
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
