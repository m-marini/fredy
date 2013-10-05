/*
 * Created on 18-nov-2004
 */
package org.mmarini.fuzzy;

import java.io.Serializable;

/**
 * @author US00852
 * @version $Id: Constant.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class Constant implements IExpression, Serializable {
	private static final long serialVersionUID = -4394085302490417618L;
	private FuzzyBoolean value = FuzzyBoolean.UNKNOWN;

	/**
	 * 
	 */
	public Constant() {
	}

	/**
	 * Creates a constants
	 * 
	 * @param value
	 *            the value
	 */
	public Constant(FuzzyBoolean value) {
		this.value = value;
	}

	/**
	 * @see org.mmarini.fuzzy.IExpression#accept(org.mmarini.fuzzy.IExpressionVisitor)
	 */
	@Override
	public void accept(IExpressionVisitor visitor) {
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object other) {
		if (this == other)
			return true;
		if (other == null)
			return false;
		if (!(other instanceof Constant))
			return false;
		return this.getValue().equals(((Constant) other).getValue());
	}

	/**
	 * @see org.mmarini.fuzzy.IExpression#evaluate(org.mmarini.fuzzy.IEvaluateContext)
	 */
	@Override
	public FuzzyBoolean evaluate(IEvaluateContext context) {
		return this.getValue();
	}

	/**
	 * @return Returns the value.
	 */
	@Override
	public FuzzyBoolean getValue() {
		return value;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return this.getValue().hashCode();
	}

	/**
	 * @see org.mmarini.fuzzy.IExpression#reset()
	 */
	@Override
	public void reset() {
	}

	/**
	 * @see org.mmarini.fuzzy.IExpression#scan(org.mmarini.fuzzy.IWeightContext)
	 */
	@Override
	public void scan(IWeightContext context) {
	}

	/**
	 * @param value
	 *            The value to set.
	 */
	public void setValue(FuzzyBoolean value) {
		this.value = value;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return String.valueOf(value);
	}
}