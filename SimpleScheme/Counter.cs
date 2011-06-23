// <copyright file="Counter.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Collections;
    using System.Collections.Generic;
    using System.Text;

    /// <summary>
    /// Handles perf counters.
    /// There is one of these in the environment.
    /// </summary>
    public class Counter : IEnumerable<string>
    {
        /// <summary>
        /// The counters are stored here.
        /// </summary>
        private Dictionary<string, int> counters = new Dictionary<string, int>();

        /// <summary>
        /// Gets a counter, given its name.
        /// </summary>
        /// <param name="name">The counter name.</param>
        /// <returns>The counter value.  If no counter exists with that name, returns -1.</returns>
        public int this[string name]
        {
            get
            {
                if (! this.counters.ContainsKey(name))
                {
                    return -1;
                }

                return this.counters[name];
            }
        }

        /// <summary>
        /// Create a new counter.
        /// If the counter already exists, just ignores the request.
        /// </summary>
        /// <param name="name">The counter name.</param>
        public void Create(string name)
        {
            if (this.counters.ContainsKey(name))
            {
                return;
            }
            
            this.counters.Add(name, 0);
        }

        /// <summary>
        /// Increment a counter, given its name.
        /// If the named counter does not exist, it is created.
        /// </summary>
        /// <param name="name">The counter name.</param>
        public void Increment(string name)
        {
            if (!this.counters.ContainsKey(name))
            {
                this.counters.Add(name, 0);
            }

            this.counters[name]++;
        }

        /// <summary>
        /// Dump the counters for reporting.
        /// </summary>
        /// <returns>The counters as a string.</returns>
        public string Dump()
        {
            StringBuilder sb = new StringBuilder();
            foreach (var kvp in this.counters)
            {
                sb.AppendFormat("{0} {1}\n", kvp.Key, kvp.Value);
            }

            return sb.ToString();
        }

        /// <summary>
        /// Reset all the counters (by deleting them).
        /// </summary>
        public void Reset()
        {
            this.counters = new Dictionary<string, int>();
        }

        /// <summary>
        /// Enumerates counter names.
        /// </summary>
        /// <returns>The counter names..</returns>
        public IEnumerator<string> GetEnumerator()
        {
            foreach (var kvp in this.counters)
            {
                yield return kvp.Key;
            }
        }

        /// <summary>
        /// Gets the counter enumerator.
        /// </summary>
        /// <returns>The string enumerator.</returns>
        IEnumerator IEnumerable.GetEnumerator()
        {
            return this.GetEnumerator();
        }
    }
}
