package org.mmarini.fuzzy;

import java.io.Serializable;

/**
 * @author US00852
 * @version $Id: Or.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class Or extends AbstractExpression implements Serializable {
	private static final long serialVersionUID = 9123613267889491834L;

	/**
	 * 
	 */
	public Or() {
	}

	/**
	 * Creates a And Expression
	 * 
	 * @param parameter
	 *            the parameter
	 */
	public Or(IExpression parameter1, IExpression parameter2) {
		super(parameter1, parameter2);
	}

	/**
	 * @see org.mmarini.fuzzy.IExpression#evaluate()
	 */
	@Override
	protected FuzzyBoolean calculateValue(IEvaluateContext context) {
		FuzzyBoolean value = FuzzyBoolean.FALSE;
		for (IExpression exp : getParameter()) {
			value = value.or(exp.evaluate(context));
		}
		return value;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object arg0) {
		if (!(arg0 instanceof Or))
			return false;
		return super.equals(arg0);
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuffer bfr = new StringBuffer();
		bfr.append("or(");
		this.copyParmsToString(bfr);
		bfr.append(")");
		return bfr.toString();
	}
}