/**
 * 
 */
package org.mmarini.fuzzy;

import org.mmarini.functional.Functor1;

/**
 * @author us00852
 * 
 */
public class AxiomInfo {
	/**
	 * 
	 */
	public final static Functor1<String, AxiomInfo> predicateGetter = new Functor1<String, AxiomInfo>() {

		@Override
		public String apply(AxiomInfo p) {
			return p.getPredicate();
		}
	};

	private String predicate;
	private int axiomsCount;
	private int hypothesisCount;

	/**
	 * @param predicate
	 * @param axiomsCount
	 * @param hypothesisCount
	 */
	public AxiomInfo(String predicate, int axiomsCount, int hypothesisCount) {
		super();
		this.predicate = predicate;
		this.axiomsCount = axiomsCount;
		this.hypothesisCount = hypothesisCount;
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
		AxiomInfo other = (AxiomInfo) obj;
		if (axiomsCount != other.axiomsCount)
			return false;
		if (hypothesisCount != other.hypothesisCount)
			return false;
		if (predicate == null) {
			if (other.predicate != null)
				return false;
		} else if (!predicate.equals(other.predicate))
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
	 * @return the predicate
	 */
	public String getPredicate() {
		return predicate;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + axiomsCount;
		result = prime * result + hypothesisCount;
		result = prime * result
				+ ((predicate == null) ? 0 : predicate.hashCode());
		return result;
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
