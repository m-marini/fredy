package org.mmarini.fuzzy;

import java.io.Serializable;

/**
 * @author US00852
 * @version $Id: FuzzyBoolean.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class FuzzyBoolean implements Serializable, Comparable<FuzzyBoolean> {
	private static final long serialVersionUID = 5080461314044438067L;
	public static final String FALSE_DESCRIPTION = "false";
	public static final String QUITE_FALSE_DESCRIPTION = "quite.false";
	public static final String UNKNOWN_DESCRIPTION = "unknown";
	public static final String QUITE_TRUE_DESCRIPTION = "quite.true";
	public static final String TRUE_DESCRIPTION = "true";

	public static final double FALSE_VALUE = 0.;
	public static final double QUITE_FALSE_VALUE = .25;
	public static final double UNKNOWN_VALUE = .5;
	public static final double QUITE_TRUE_VALUE = .75;
	public static final double TRUE_VALUE = 1.;

	public final static FuzzyBoolean FALSE = new FuzzyBoolean(FALSE_VALUE);
	public final static FuzzyBoolean QUITE_FALSE = new FuzzyBoolean(
			QUITE_FALSE_VALUE);
	public final static FuzzyBoolean UNKNOWN = new FuzzyBoolean(UNKNOWN_VALUE);
	public final static FuzzyBoolean QUITE_TRUE = new FuzzyBoolean(
			QUITE_TRUE_VALUE);
	public final static FuzzyBoolean TRUE = new FuzzyBoolean(TRUE_VALUE);

	private double value;

	/**
	 * 
	 */
	public FuzzyBoolean() {
	}

	/**
	 * Creates a FuzzyValue
	 * 
	 * @param value
	 *            the value
	 */
	public FuzzyBoolean(double value) {
		this.value = value;
	}

	/**
	 * Creates a FuzzyValue
	 * 
	 * @param value
	 *            the value
	 */
	public FuzzyBoolean(FuzzyBoolean value) {
		this.value = value.value;
	}

	/**
	 * Returns the and value
	 * 
	 * @param value
	 *            the value
	 * @return the and value
	 */
	public FuzzyBoolean and(FuzzyBoolean value) {
		double exp = Math.min(this.value, value.value);
		return new FuzzyBoolean(exp);
	}

	/**
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(FuzzyBoolean value) {
		if (equals(value))
			return 0;
		if (this.value > value.value)
			return 1;
		if (this.value < value.value)
			return -1;
		return 0;
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
		if (!(other instanceof FuzzyBoolean))
			return false;
		return value == ((FuzzyBoolean) other).value;
	}

	/**
	 * Returns the description
	 * 
	 * @return the description
	 */
	public String getDescription() {
		if (isFalse())
			return FALSE_DESCRIPTION;
		else if (isQuiteFalse())
			return QUITE_FALSE_DESCRIPTION;
		else if (isUnknown())
			return UNKNOWN_DESCRIPTION;
		else if (isQuiteTrue())
			return QUITE_TRUE_DESCRIPTION;
		else
			return TRUE_DESCRIPTION;
	}

	/**
	 * Returns the value
	 * 
	 * @return Returns the value.
	 */
	public double getValue() {
		return value;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return new Double(value).hashCode();
	}

	/**
	 * Returns the ifonlyif value
	 * 
	 * @param value
	 *            the value
	 * @return the ifonlyif value
	 */
	public FuzzyBoolean ifonlyif(FuzzyBoolean value) {
		double exp = 1 - Math.abs(this.value - value.value);
		return new FuzzyBoolean(exp);
	}

	/**
	 * Returns the implies value
	 * 
	 * @param value
	 *            the value
	 * @return the implies value
	 */
	public FuzzyBoolean implies(FuzzyBoolean value) {
		double exp = Math.min(1 - this.value + value.value, 1.);
		return new FuzzyBoolean(exp);
	}

	/**
	 * Returns true if value is false
	 * 
	 * @return true if value is false
	 */
	public boolean isFalse() {
		return value <= 0.;
	}

	/**
	 * Returns true if values is known (true or false)
	 * 
	 * @return true if values is known (true or false)
	 */
	public boolean isKnown() {
		return isFalse() || isTrue();
	}

	/**
	 * Returns true if value is quite false
	 * 
	 * @return true if value is quite false
	 */
	public boolean isQuiteFalse() {
		return value > 0. && value <= 1. / 3.;
	}

	/**
	 * Returns true if value is quite true
	 * 
	 * @return true if value is quite true
	 */
	public boolean isQuiteTrue() {
		return value > 2. / 3. && value < 1.;
	}

	/**
	 * Returns true if value is quite true
	 * 
	 * @return true if value is quite true
	 */
	public boolean isTrue() {
		return value >= 1.;
	}

	/**
	 * Returns true if value is half true
	 * 
	 * @return true if value is half true
	 */
	public boolean isUnknown() {
		return value > 1.0 / 3.0 && value <= 2.0 / 3.0;
	}

	/**
	 * Returns the known value
	 * 
	 * @return the known value
	 */
	public FuzzyBoolean known() {
		return new FuzzyBoolean(2.0 * Math.abs(value - 0.5));
	}

	/**
	 * Returns the negation
	 * 
	 * @return the negation
	 */
	public FuzzyBoolean not() {
		return new FuzzyBoolean(1. - value);
	}

	/**
	 * Returns the or value
	 * 
	 * @param value
	 *            the value
	 * @return the or value
	 */
	public FuzzyBoolean or(FuzzyBoolean value) {
		double exp = Math.max(this.value, value.value);
		return new FuzzyBoolean(exp);
	}

	/**
	 * Returns the somewhat value
	 * 
	 * @return the somewhat
	 */
	public FuzzyBoolean somewhat() {
		return new FuzzyBoolean(Math.sqrt(value));
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder value = new StringBuilder();
		return value.append(getDescription()).append("(").append(this.value)
				.append(")").toString();
	}

	/**
	 * Returns the very value
	 * 
	 * @return the very value
	 */
	public FuzzyBoolean very() {
		return new FuzzyBoolean(value * value);
	}
}