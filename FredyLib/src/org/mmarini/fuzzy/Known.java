package org.mmarini.fuzzy;

import java.io.Serializable;

/**
 * @author US00852
 * @version $Id: Known.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class Known extends AbstractExpression implements Serializable {

	/**
	 * Creates a Not Expression
	 * 
	 * @param parameter
	 *            the parameter
	 */
	public Known(IExpression parameter) {
		super(parameter);
	}

	/**
	 * @see org.mmarini.fuzzy.IExpression#evaluate()
	 */
	@Override
	protected FuzzyBoolean calculateValue(IEvaluateContext context) {
		return this.getParameter(0).evaluate(context).known();
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object arg0) {
		if (!(arg0 instanceof Known))
			return false;
		return super.equals(arg0);
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuffer bfr = new StringBuffer();
		bfr.append("known(");
		this.copyParmsToString(bfr);
		bfr.append(")");
		return bfr.toString();
	}
}