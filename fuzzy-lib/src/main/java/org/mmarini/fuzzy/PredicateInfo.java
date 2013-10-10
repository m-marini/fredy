/**
 * 
 */
package org.mmarini.fuzzy;

/**
 * @author us00852
 * 
 */
public class PredicateInfo {
	private String predicate;
	private int axiomsCount;
	private int hypothesisCount;

	/**
	 * @param predicate
	 * @param axiomsCount
	 * @param hypothesisCount
	 */
	public PredicateInfo(String predicate, int axiomsCount, int hypothesisCount) {
		super();
		this.predicate = predicate;
		this.axiomsCount = axiomsCount;
		this.hypothesisCount = hypothesisCount;
	}

	/**
	 * @return the predicate
	 */
	public String getPredicate() {
		return predicate;
	}

	/**
	 * @return the axiomsCount
	 */
	public int getAxiomsCount() {
		return axiomsCount;
	}

	/**
	 * @return the hypothesisCount
	 */
	public int getHypothesisCount() {
		return hypothesisCount;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("PredicateInfo [predicate=").append(predicate)
				.append(", axiomsCount=").append(axiomsCount)
				.append(", hypothesisCount=").append(hypothesisCount)
				.append("]");
		return builder.toString();
	}

}
