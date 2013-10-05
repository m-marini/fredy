package org.mmarini.fuzzy;

import java.io.Serializable;

/**
 * It rapresents a predicate with its name, value and weight.
 * 
 * @author US00852
 * @version $Id: Predicate.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class Predicate implements IPredicate, Serializable {
	private static final long serialVersionUID = 8656002102982259721L;
	private FuzzyBoolean value;
	private String name;
	private Weight weight;

	/**
	 * 
	 */
	public Predicate() {
		value = FuzzyBoolean.UNKNOWN;
		weight = Weight.NULL_WEIGHT;
	}

	/**
	 * Creates a predicate
	 * 
	 * @param name
	 *            the name
	 */
	public Predicate(String name) {
		this();
		this.name = name;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Predicate other = (Predicate) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		if (weight == null) {
			if (other.weight != null)
				return false;
		} else if (!weight.equals(other.weight))
			return false;
		return true;
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
		final int prime = 31;
		int result = 1;
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + ((value == null) ? 0 : value.hashCode());
		result = prime * result + ((weight == null) ? 0 : weight.hashCode());
		return result;
	}

	/**
	 * @see org.mmarini.fuzzy.IPredicate#reset()
	 */
	@Override
	public void reset() {
		value = FuzzyBoolean.UNKNOWN;
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
	 * @see org.mmarini.fuzzy.IPredicate#setWeight(org.mmarini.fuzzy.Weight)
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
		StringBuilder builder = new StringBuilder();
		builder.append("Predicate [name=").append(name).append(", value=")
				.append(value).append(", weight=").append(weight).append("]");
		return builder.toString();
	}
}