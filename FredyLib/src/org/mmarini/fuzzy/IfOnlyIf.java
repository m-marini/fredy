package org.mmarini.fuzzy;

import java.io.Serializable;

/**
 * @author US00852
 * @version $Id: IfOnlyIf.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class IfOnlyIf extends AbstractExpression implements Serializable {

	/**
	 * Creates a And Expression
	 * 
	 * @param parameter
	 *            the parameter
	 */
	public IfOnlyIf(IExpression parameter1, IExpression parameter2) {
		super(parameter1, parameter2);
	}

	/**
	 * @see org.mmarini.fuzzy.IExpression#evaluate()
	 */
	@Override
	protected FuzzyBoolean calculateValue(IEvaluateContext context) {
		return this.getParameter(0).evaluate(context)
				.ifonlyif(this.getParameter(1).evaluate(context));
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object arg0) {
		if (!(arg0 instanceof IfOnlyIf))
			return false;
		return super.equals(arg0);
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuffer bfr = new StringBuffer();
		bfr.append("ifonlyif(");
		this.copyParmsToString(bfr);
		bfr.append(")");
		return bfr.toString();
	}
}