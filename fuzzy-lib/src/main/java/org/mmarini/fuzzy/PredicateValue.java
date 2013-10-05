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

	/**
	 * @return the predicate
	 */
	public String getPredicate() {
		return predicate;
	}

	/**
	 * @param predicate
	 *            the predicate to set
	 */
	public void setPredicate(String predicate) {
		this.predicate = predicate;
	}

	/**
	 * @return the value
	 */
	public FuzzyBoolean getValue() {
		return value;
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
