package org.mmarini.fuzzy;

/**
 * @author US00852
 * @version $Id: Weight.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class Weight implements Comparable {
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
		return new Weight(this.getWeight() + other.getWeight(),
				this.getDependeceCount() + other.getDependeceCount());
	}

	/**
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(Object other) {
		Weight weight = (Weight) other;
		int diff = this.getWeight() - weight.getWeight();
		if (diff != 0)
			return diff;
		return weight.getDependeceCount() - this.getDependeceCount();
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object other) {
		if (other == this)
			return true;
		if (other == null)
			return false;
		if (!(other instanceof Weight))
			return false;
		Weight weight = (Weight) other;
		if (this.getWeight() != weight.getWeight())
			return false;
		if (this.getDependeceCount() != weight.getDependeceCount())
			return false;
		return true;
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
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return this.getWeight() * 31 + this.getDependeceCount();
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
		StringBuffer bfr = new StringBuffer();
		// bfr.append(this.getClass().getName());
		bfr.append("(w=");
		bfr.append(this.getWeight());
		bfr.append(",d=");
		bfr.append(this.getDependeceCount());
		bfr.append(")");
		return bfr.toString();
	}
}