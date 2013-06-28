package org.mmarini.fuzzy;

import java.io.Serializable;

/**
 * @author US00852
 * @version $Id: PredicateExpression.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class PredicateExpression implements IExpression, Serializable {
	private IPredicate predicate;

	/**
	 * Creates a PredicateExpression
	 * 
	 * @param predicate
	 *            the predicate
	 */
	public PredicateExpression(IPredicate predicate) {
		this.predicate = predicate;
	}

	/**
	 * @see org.mmarini.fuzzy.IExpression#accept(org.mmarini.fuzzy.IExpressionVisitor)
	 */
	@Override
	public void accept(IExpressionVisitor visitor) {
		visitor.visitPredicate(this.getPredicate());
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
		if (!(other instanceof PredicateExpression))
			return false;
		Object obj = this.getPredicate();
		Object predicate = ((PredicateExpression) other).getPredicate();
		return obj == null ? predicate == null : obj.equals(predicate);
	}

	/**
	 * @see org.mmarini.fuzzy.IExpression#evaluate(org.mmarini.fuzzy.IEvaluateContext)
	 */
	@Override
	public FuzzyBoolean evaluate(IEvaluateContext context) {
		return context.evaluate(this.getPredicate());
	}

	/**
	 * Returns the predicate.
	 * 
	 * @return the predicate.
	 */
	public IPredicate getPredicate() {
		return predicate;
	}

	/**
	 * @see org.mmarini.fuzzy.IExpression#getValue()
	 */
	@Override
	public FuzzyBoolean getValue() {
		return this.getPredicate().getValue();
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		Object obj = this.getPredicate();
		return obj == null ? 0 : obj.hashCode();
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
		if (!FuzzyBoolean.UNKNOWN.equals(this.getValue()))
			return;
		context.scan(context, this.getPredicate());
	}

	/**
	 * Sets the predicate
	 * 
	 * @param predicate
	 *            the predicate to set.
	 */
	public void setPredicate(IPredicate predicate) {
		this.predicate = predicate;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuffer bfr = new StringBuffer();
		bfr.append("(");
		bfr.append(this.getPredicate().getName());
		bfr.append(")");
		return bfr.toString();
	}
}