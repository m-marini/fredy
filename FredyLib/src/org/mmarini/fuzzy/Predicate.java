package org.mmarini.fuzzy;

import java.io.Serializable;

/**
 * It rapresents a predicate with its name, value and weight.
 * 
 * @author US00852
 * @version $Id: Predicate.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class Predicate implements IPredicate, Serializable {
	private FuzzyBoolean value = FuzzyBoolean.UNKNOWN;
	private String name;
	private Weight weight = Weight.NULL_WEIGHT;

	/**
	 * 
	 */
	public Predicate() {
	}

	/**
	 * Creates a predicate
	 * 
	 * @param name
	 *            the name
	 */
	public Predicate(String name) {
		this.name = name;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object value) {
		if (value == this)
			return true;
		if (value == null)
			return false;
		if (!(value instanceof Predicate))
			return false;
		String name = this.getName();
		return name == null ? ((Predicate) value).getName() == null : name
				.equals(((Predicate) value).getName());
	}

	/**
	 * @see org.mmarini.fuzzy.IPredicate#getName()
	 */
	@Override
	public String getName() {
		return name;
	}

	/**
	 * Returns the value
	 * 
	 * @return the value.
	 */
	@Override
	public FuzzyBoolean getValue() {
		return value;
	}

	/**
	 * Returns the weight
	 * 
	 * @return the weight.
	 */
	@Override
	public Weight getWeight() {
		return weight;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		int hashCode = 0;
		Object obj = this.getName();
		if (obj != null)
			hashCode = name.hashCode();
		return hashCode;
	}

	/**
	 * @see org.mmarini.fuzzy.IPredicate#reset()
	 */
	@Override
	public void reset() {
		this.setValue(FuzzyBoolean.UNKNOWN);
	}

	/**
	 * Sets the value
	 * 
	 * @param value
	 *            the value to set.
	 */
	@Override
	public void setValue(FuzzyBoolean value) {
		this.value = value;
	}

	/**
	 * Sets the weight
	 * 
	 * @param weight
	 *            the weight to set.
	 */
	@Override
	public void setWeight(Weight weight) {
		this.weight = weight;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuffer value = new StringBuffer();
		value.append("Predicate(");
		value.append(this.getName());
		value.append(",");
		value.append(this.getValue());
		value.append(")");
		return value.toString();
	}
}