package org.mmarini.fuzzy;

import java.io.Serializable;

/**
 * @author US00852
 * @version $Id: And.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class And extends AbstractExpression implements Serializable {
	private static final long serialVersionUID = -6124571758068883560L;

	/**
	 * 
	 */
	public And() {
	}

	/**
	 * Creates a And Expression
	 * 
	 * @param parameter
	 *            the parameter
	 */
	public And(IExpression parameter1, IExpression parameter2) {
		super(parameter1, parameter2);
	}

	/**
	 * @see org.mmarini.fuzzy.IExpression#evaluate()
	 */
	@Override
	public FuzzyBoolean calculateValue(IEvaluateContext context) {
		FuzzyBoolean value = FuzzyBoolean.TRUE;
		for (IExpression exp : getParameter()) {
			value = value.and(exp.evaluate(context));
		}
		return value;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object arg0) {
		if (!(arg0 instanceof And))
			return false;
		return super.equals(arg0);
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuffer bfr = new StringBuffer();
		bfr.append("and(");
		this.copyParmsToString(bfr);
		bfr.append(")");
		return bfr.toString();
	}
}