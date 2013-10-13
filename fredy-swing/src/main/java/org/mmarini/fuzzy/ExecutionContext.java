/**
 * 
 */
package org.mmarini.fuzzy;

/**
 * @author US00852
 * 
 */
public interface ExecutionContext {

	/**
	 * Assign the value to a predicate
	 * 
	 * @param predicate
	 */
	public abstract void assertFalse(String predicate);

	/**
	 * Assign the value to a predicate
	 * 
	 * @param predicate
	 */
	public abstract void assertTrue(String predicate);

	/**
	 * Pop a value from context
	 * 
	 * @return the value
	 */
	public abstract FuzzyBoolean pop();

	/**
	 * Push a value into context
	 * 
	 * @param value
	 *            the value
	 */
	public abstract void push(FuzzyBoolean value);

	/**
	 * Push the value of a predicate into context
	 * 
	 * @param predicate
	 *            the predicate
	 */
	public abstract void push(String predicate);
}
