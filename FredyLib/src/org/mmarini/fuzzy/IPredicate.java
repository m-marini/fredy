package org.mmarini.fuzzy;

/**
 * @author US00852
 * @version $Id: IPredicate.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public interface IPredicate {

	/**
	 * Returns the name
	 * 
	 * @return the name
	 */
	public abstract String getName();

	/**
	 * Returns the value
	 * 
	 * @return the value
	 */
	public abstract FuzzyBoolean getValue();

	/**
	 * Returns the weight
	 * 
	 * @return the weight
	 */
	public abstract Weight getWeight();

	/**
	 * Resets the predicate
	 */
	public abstract void reset();

	/**
	 * Sets the value
	 * 
	 * @param value
	 *            the value
	 */
	public abstract void setValue(FuzzyBoolean value);

	/**
	 * Sets the weight
	 * 
	 * @param weight
	 *            the weight
	 */
	public abstract void setWeight(Weight weight);
}