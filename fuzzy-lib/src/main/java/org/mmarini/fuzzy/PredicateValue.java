/**
 * 
 */
package org.mmarini.fuzzy;

/**
 * @author us00852
 * 
 */
public class PredicateValue {
	private String predicate;
	private FuzzyBoolean value;

	/**
	 * 
	 */
	public PredicateValue() {
	}

	/**
	 * 
	 * @param predicate
	 * @param value
	 */
	public PredicateValue(String predicate, FuzzyBoolean value) {
		this.predicate = predicate;
		this.value = value;
	}

	/*
	 * (non-Javadoc)
	 * 
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
		PredicateValue other = (PredicateValue) obj;
		if (predicate == null) {
			if (other.predicate != null)
				return false;
		} else if (!predicate.equals(other.predicate))
			return false;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}

	/**
	 * @return the predicate
	 */
	public String getPredicate() {
		return predicate;
	}

	/**
	 * @return the value
	 */
	public FuzzyBoolean getValue() {
		return value;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((predicate == null) ? 0 : predicate.hashCode());
		result = prime * result + ((value == null) ? 0 : value.hashCode());
		return result;
	}

	/**
	 * @param predicate
	 *            the predicate to set
	 */
	public void setPredicate(String predicate) {
		this.predicate = predicate;
	}

	/**
	 * @param value
	 *            the value to set
	 */
	public void setValue(FuzzyBoolean value) {
		this.value = value;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("PredicateValue [predicate=").append(predicate)
				.append(", value=").append(value).append("]");
		return builder.toString();
	}

}
