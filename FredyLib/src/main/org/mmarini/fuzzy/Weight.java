package org.mmarini.fuzzy;

/**
 * @author US00852
 * @version $Id: Weight.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class Weight implements Comparable<Weight> {
	public static final Weight NULL_WEIGHT = new Weight();

	private int weight;
	private int dependeceCount;

	/**
	 * 
	 */
	public Weight() {
	}

	/**
	 * Create a Weight
	 * 
	 * @param weight
	 *            the weight
	 * @param dependeceCount
	 *            del dependence count
	 */
	public Weight(int weight, int dependeceCount) {
		this.weight = weight;
		this.dependeceCount = dependeceCount;
	}

	/**
	 * Calculates the add value
	 * 
	 * @param other
	 *            the other weight
	 * @return the add value
	 */
	public Weight add(Weight other) {
		return new Weight(weight + other.weight, dependeceCount
				+ other.dependeceCount);
	}

	/**
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(Weight weight) {
		int diff = this.weight - weight.weight;
		if (diff != 0)
			return diff;
		return weight.dependeceCount - dependeceCount;
	}

	/**
	 * Returns the dependeceCount.
	 * 
	 * @return the dependeceCount.
	 */
	public int getDependeceCount() {
		return dependeceCount;
	}

	/**
	 * Returns the weight.
	 * 
	 * @return the weight.
	 */
	public int getWeight() {
		return weight;
	}

	/**
	 * Returns the maximu value between this and other
	 * 
	 * @param other
	 *            the other value
	 * @return the maximun value
	 */
	public Weight max(Weight other) {
		if (this.compareTo(other) < 0)
			return other;
		return this;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + dependeceCount;
		result = prime * result + weight;
		return result;
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
		Weight other = (Weight) obj;
		if (dependeceCount != other.dependeceCount)
			return false;
		if (weight != other.weight)
			return false;
		return true;
	}

	/**
	 * Returns the minimum value between this and other
	 * 
	 * @param other
	 *            the other value
	 * @returnthe minimum value
	 */
	public Weight min(Weight other) {
		if (this.compareTo(other) > 0)
			return other;
		return this;
	}

	/**
	 * Set the dependeceCount
	 * 
	 * @param dependeceCount
	 *            the dependeceCount to set.
	 */
	public void setDependeceCount(int dependeceCount) {
		this.dependeceCount = dependeceCount;
	}

	/**
	 * Sets the weight
	 * 
	 * @param weight
	 *            the weight to set.
	 */
	public void setWeight(int weight) {
		this.weight = weight;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder bfr = new StringBuilder();
		return bfr.append("(w=").append(this.getWeight()).append(",d=")
				.append(this.getDependeceCount()).append(")").toString();
	}
}