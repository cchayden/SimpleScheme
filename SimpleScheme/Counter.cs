// <copyright file="Counter.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Collections.Generic;
    using System.Diagnostics.Contracts;
    using System.Linq;
    using System.Text;

    /// <summary>
    /// Handles perf counters.
    /// There is one of these in each interpreter.
    /// They all share a dictionary that maps a name to a counter id.
    /// The counter id is used as an index into a counter array.
    /// An array is used because accessing through a dictionary is relatively slow.
    /// </summary>
    internal class Counter
    {
        #region Fields
        /// <summary>
        /// The actual counters.
        /// </summary>
        private Dictionary<string, int> counters = new Dictionary<string, int>();
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the counter primitives.
        /// </summary>
        /// <param name="primEnv">The environment to define the primitives into.</param>
        internal static void DefinePrimitives(PrimitiveEnvironment primEnv)
        {
            Contract.Requires(primEnv != null);
            primEnv
                .DefinePrimitive(
                    "dump-counters", 
                    new[] { "(dump-counters)" },
                    (args, env, caller) => caller.Interp.CurrentCounters.DumpCounters(caller.Interp.CurrentOutputPort), 
                    new ArgsInfo(0))
                .DefinePrimitive(
                    "get-counters", 
                    new[] { "(get-counters)" },
                    (args, env, caller) => caller.Interp.CurrentCounters.GetCounters(), 
                    new ArgsInfo(0))
                .DefinePrimitive(
                    "get-counter", 
                    new[] { "(get-counter <name>)" },
                    (args, env, caller) => caller.Interp.CurrentCounters.GetCounter(List.First(args)), 
                    new ArgsInfo(1, ArgType.String))
                .DefinePrimitive(
                    "reset-counters", 
                    new[] { "(reset-counters)" },
                    (args, env, caller) => caller.Interp.CurrentCounters.ResetCounters(), 
                    new ArgsInfo(0));
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Increment a counter, given its id.
        /// </summary>
        /// <param name="counter">The counter name.</param>
        internal void Increment(string counter)
        {
            lock(this)
            {
                if (counters.ContainsKey(counter))
                {
                    this.counters[counter]++;
                }
                else
                {
                    counters[counter] = 1;
                }
            }
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Dump the counters on the console.
        /// </summary>
        /// <param name="port">The port to dump to.</param>
        /// <returns>The result is unspecified.</returns>
        private SchemeObject DumpCounters(OutputPort port)
        {
            Contract.Requires(port != null);
            var sb = new StringBuilder();
            this.Dump(sb);
            port.WriteLine(sb.ToString());
            return Undefined.Instance;
        }

        /// <summary>
        /// Get the counters, as a list of name/count pairs.
        /// </summary>
        /// <returns>The list of counterName/count pairs.</returns>
        private SchemeObject GetCounters()
        {
            SchemeObject res = EmptyList.Instance;
            lock (this)
            {
                foreach (var kvp in counters)
                {
                    int count = kvp.Value;
                    if (count > 0)
                    {
                        Contract.Assume(kvp.Key != null);
                        res = List.Cons(List.Cons((Symbol)(kvp.Key), (Number)count), res);
                    }
                }
            }
            return res;
        }

        /// <summary>
        /// Get an individual counter value.
        /// </summary>
        /// <param name="name">The counter name.</param>
        /// <returns>The counter value.</returns>
        private SchemeObject GetCounter(SchemeObject name)
        {
            Contract.Requires(name != null);
            string counterName = name.ToString();
            lock (this)
            {
                foreach (var kvp in counters)
                {
                    if (kvp.Key == counterName)
                    {
                        return (Number)kvp.Value;
                    }

                }
            }

            return Undefined.Instance;
        }

        /// <summary>
        /// Dump the counter names and corresponding values into a 
        ///  StringBuilder for reporting.
        /// </summary>
        /// <param name="sb">The string builder to dump to.</param>
        private void Dump(StringBuilder sb)
        {
            Contract.Requires(sb != null);
            lock (this)
            {
                foreach (var kvp in counters.OrderBy(elem => elem.Key))
                {
                    int count = kvp.Value;
                    if (count > 0)
                    {
                        sb.AppendFormat("{0} {1}\n", kvp.Key, count);
                    }
                }
            }
        }

        /// <summary>
        /// Reset all the counters.
        /// Do not delete counter names.
        /// </summary>
        /// <returns>The result is unspecified.</returns>
        private SchemeObject ResetCounters()
        {
            lock (this)
            {
                this.counters = new Dictionary<string, int>();
            }

            return Undefined.Instance;
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.counters != null);
        }
        #endregion
    }
}
