package org.mmarini.fuzzy;

import java.io.Serializable;

/**
 * @author US00852
 * @version $Id: Somewhat.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class Somewhat extends AbstractExpression implements Serializable {
	private static final long serialVersionUID = -5143810053313567832L;

	/**
	 * Creates a Not Expression
	 * 
	 * @param parameter
	 *            the parameter
	 */
	public Somewhat(IExpression parameter) {
		super(parameter);
	}

	/**
	 * @see org.mmarini.fuzzy.IExpression#evaluate()
	 */
	@Override
	protected FuzzyBoolean calculateValue(IEvaluateContext context) {
		return this.getParameter(0).evaluate(context).somewhat();
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object arg0) {
		if (!(arg0 instanceof Somewhat))
			return false;
		return super.equals(arg0);
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuffer bfr = new StringBuffer();
		bfr.append("somewhat(");
		this.copyParmsToString(bfr);
		bfr.append(")");
		return bfr.toString();
	}
}