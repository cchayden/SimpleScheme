// <copyright file="Counter.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Collections.Generic;
    using System.Diagnostics.Contracts;
    using System.Text;
    using System.Threading;

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
        /// The maximum number of counters supported.
        /// Increase this if more counters are added and the increment function
        ///   starts throwing index out of bounds exceptions.
        /// It would be better if this was determined automatically, but that is expensive.
        /// </summary>
        private const int MaxCounters = 30;

        /// <summary>
        /// The counterNames are stored here.
        /// </summary>
        private static readonly Dictionary<string, int> counterNames = new Dictionary<string, int>();

        /// <summary>
        /// The actual counters.
        /// </summary>
        private readonly int[] counters;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Counter class.
        /// Creates a new counter for each of the counter names.
        /// Counter names should all be created before instances are created.
        /// </summary>
        internal Counter()
        {
            Contract.Ensures(Contract.ForAll(this.counters, elem => elem == 0));
            this.counters = new int[MaxCounters];
        }
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

        #region Internal Static Methods
        /// <summary>
        /// Create a new counter id, given its name.
        /// If the counter already exists, return its id.
        /// Counters are normally created by static constructors, which execute at 
        ///   unpredictable times.  So it is impossible to know when all counters have
        ///   been created.  This is what makes it impossible to allocate the counter
        ///   array itself.
        /// </summary>
        /// <param name="name">The counter name.</param>
        /// <returns>The counter id.</returns>
        internal static int Create(string name)
        {
            Contract.Ensures(Contract.Result<int>() >= 0);
            lock (counterNames)
            {
                if (counterNames.ContainsKey(name))
                {
                    Contract.Assume(counterNames[name] >= 0);
                    return counterNames[name];
                }

                int num = counterNames.Count;
                Contract.Assert(num >= 0);
                counterNames.Add(name, num);
                return num;
            }
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Increment a counter, given its id.
        /// </summary>
        /// <param name="id">The counter id.</param>
        internal void Increment(int id)
        {
            Contract.Requires(id >= 0);
            Contract.Ensures(this.counters[id] >= 0);
            Contract.Assume(id < this.counters.Length);
            Contract.Assert(this.counters != null);
            Interlocked.Increment(ref this.counters[id]);
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
            foreach (var kvp in counterNames)
            {
                int count = this.counters[kvp.Value];
                if (count > 0)
                {
                    Contract.Assume(kvp.Key != null);
                    res = List.Cons(List.Cons((Symbol)kvp.Key, (Number)count), res);
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
            if (counterNames.ContainsKey(counterName))
            {
                var index = counterNames[counterName];
                Contract.Assume(index >= 0);
                Contract.Assume(index < this.counters.Length);
                return (Number)this.counters[index];
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
            foreach (var kvp in counterNames)
            {
                int count = this.counters[kvp.Value];
                if (count > 0)
                {
                    sb.AppendFormat("{0} {1}\n", kvp.Key, count);
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
            Contract.Ensures(Contract.ForAll(this.counters, elem => elem == 0));
            for (int i = 0; i < this.counters.Length; i++)
            {
                this.counters[i] = 0;
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
