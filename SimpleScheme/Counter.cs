﻿// <copyright file="Counter.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Collections.Generic;
    using System.Text;
    using System.Threading;
    using Obj = System.Object;

    /// <summary>
    /// Handles perf counters.
    /// There is one of these in each interpreter.
    /// They all share a dictionary that maps a name to a counter id.
    /// The counter id is used as an index into a counter array.
    /// An array is used because accessing through a dictionary is relatively slow.
    /// </summary>
    public class Counter
    {
        #region Fields
        /// <summary>
        /// The counterNames are stored here.
        /// </summary>
        private static readonly Dictionary<string, int> counterNames = new Dictionary<string, int>();

        /// <summary>
        /// The maximum number of counters supported.
        /// Increase this if more counters are added and the increment function
        ///   starts throwing index out of bounds exceptions.
        /// It would be better if this was determined automatically, but that is expensive.
        /// </summary>
        private const int MaxCounters = 30;

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
        public Counter()
        {
            this.counters = new int[MaxCounters];
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
                //// (dump-counters)
                .DefinePrimitive("dump-counters", (args, caller) => caller.Interp.CurrentCounters.DumpCounters(caller.Interp.CurrentOutputPort), 0)
                //// (get-counters)
                .DefinePrimitive("get-counters", (args, caller) => caller.Interp.CurrentCounters.GetCounters(), 0)
                //// (get-counter <name>)
                .DefinePrimitive("get-counter", (args, caller) => caller.Interp.CurrentCounters.GetCounter(List.First(args)), 1, Primitive.ValueType.String)
                //// (reset-counters)
                .DefinePrimitive("reset-counters", (args, caller) => caller.Interp.CurrentCounters.ResetCounters(), 0);
        }
        #endregion

        #region Public Static Methods
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
        public static int Create(string name)
        {
            lock (counterNames)
            {
                if (counterNames.ContainsKey(name))
                {
                    return counterNames[name];
                }

                int num = counterNames.Count;
                counterNames.Add(name, num);
                return num;
            }
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Increment a counter, given its id.
        /// </summary>
        /// <param name="id">The counter id.</param>
        public void Increment(int id)
        {
            Interlocked.Increment(ref this.counters[id]);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Dump the counters on the console.
        /// </summary>
        /// <param name="port">The port to dump to.</param>
        /// <returns>The result is unspecified.</returns>
        private Obj DumpCounters(OutputPort port)
        {
            StringBuilder sb = new StringBuilder();
            this.Dump(sb);
            port.WriteLine(sb.ToString());
            return new Undefined();
        }

        /// <summary>
        /// Get the counters, as a list of name/count pairs.
        /// </summary>
        /// <returns>The list of counterName/count pairs.</returns>
        private Obj GetCounters()
        {
            Obj res = EmptyList.Instance;
            foreach (var kvp in counterNames)
            {
                int count = this.counters[kvp.Value];
                if (count > 0)
                {
                    res = Pair.Cons(Pair.Cons(kvp.Key, count), res);
                }
            }

            return res;
        }

        /// <summary>
        /// Get an individual counter value.
        /// </summary>
        /// <param name="name">The counter name.</param>
        /// <returns>The counter value.</returns>
        private Obj GetCounter(Obj name)
        {
            string counterName = name.ToString();
            if (counterNames.ContainsKey(counterName))
            {
                return this.counters[counterNames[counterName]];
            }

            return new Undefined();
        }

        /// <summary>
        /// Dump the counter names and corresponding values into a 
        ///  StringBuilder for reporting.
        /// </summary>
        /// <param name="sb">The string builder to dump to.</param>
        private void Dump(StringBuilder sb)
        {
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
        private Obj ResetCounters()
        {
            for (int i = 0; i < this.counters.Length; i++)
            {
                this.counters[i] = 0;
            }

            return new Undefined();
        }
        #endregion
    }
}
