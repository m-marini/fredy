package org.mmarini.fuzzy;

import java.io.Serializable;

/**
 * @author US00852
 * @version $Id: Not.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class Not extends AbstractExpression implements Serializable {
	private static final long serialVersionUID = -8481295878307981120L;

	/**
	 * Creates a Not Expression
	 * 
	 * @param parameter
	 *            the parameter
	 */
	public Not(IExpression parameter) {
		super(parameter);
	}

	/**
	 * @see org.mmarini.fuzzy.IExpression#evaluate()
	 */
	@Override
	protected FuzzyBoolean calculateValue(IEvaluateContext context) {
		return this.getParameter(0).evaluate(context).not();
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object other) {
		if (!(other instanceof Not))
			return false;
		return super.equals(other);
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuffer bfr = new StringBuffer();
		bfr.append("not(");
		this.copyParmsToString(bfr);
		bfr.append(")");
		return bfr.toString();
	}
}