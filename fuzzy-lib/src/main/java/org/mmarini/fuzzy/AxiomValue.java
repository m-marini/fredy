/**
 * 
 */
package org.mmarini.fuzzy;

/**
 * @author us00852
 * 
 */
public class AxiomValue extends PredicateValue {
	private int axiomsCount;
	private int hypothesisCount;

	/**
	 * 
	 */
	public AxiomValue() {
	}

	/**
	 * 
	 * @param predicate
	 * @param axiomCount
	 * @param hypothesisCount
	 * @param value
	 */
	public AxiomValue(AxiomInfo info, FuzzyBoolean value) {
		this(info.getPredicate(), info.getAxiomsCount(), info
				.getHypothesisCount(), value);
	}

	/**
	 * 
	 * @param predicate
	 * @param axiomCount
	 * @param hypothesisCount
	 * @param value
	 */
	public AxiomValue(String predicate, int axiomCount, int hypothesisCount,
			FuzzyBoolean value) {
		super(predicate, value);
		this.axiomsCount = axiomCount;
		this.hypothesisCount = hypothesisCount;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		AxiomValue other = (AxiomValue) obj;
		if (axiomsCount != other.axiomsCount)
			return false;
		if (hypothesisCount != other.hypothesisCount)
			return false;
		return true;
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

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + axiomsCount;
		result = prime * result + hypothesisCount;
		return result;
	}

	/**
	 * @param axiomsCount
	 *            the axiomsCount to set
	 */
	public void setAxiomsCount(int axiomsCount) {
		this.axiomsCount = axiomsCount;
	}

	/**
	 * @param hypothesisCount
	 *            the hypothesisCount to set
	 */
	public void setHypothesisCount(int hypothesisCount) {
		this.hypothesisCount = hypothesisCount;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("AxiomValue [predicate=").append(getPredicate())
				.append(", value=").append(getValue()).append(", axiomsCount=")
				.append(axiomsCount).append(", hypothesisCount=")
				.append(hypothesisCount).append("]");
		return builder.toString();
	}

}
