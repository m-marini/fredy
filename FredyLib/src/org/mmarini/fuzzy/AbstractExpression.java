package org.mmarini.fuzzy;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * @author US00852
 * @version $Id: AbstractExpression.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public abstract class AbstractExpression implements IExpression {
	private List parameters = new ArrayList(2);
	private FuzzyBoolean value;

	/**
	 * 
	 */
	public AbstractExpression() {
	}

	/**
	 * Create an unary expression
	 * 
	 * @param parameter
	 *            the parameter
	 */
	public AbstractExpression(IExpression parameter) {
		this.getParameter().add(parameter);
	}

	/**
	 * Create a binary expression
	 * 
	 * @param parameter1
	 *            the first parameter
	 * @param parameter2
	 *            the first parameter
	 */
	public AbstractExpression(IExpression parameter1, IExpression parameter2) {
		this.getParameter().add(parameter1);
		this.getParameter().add(parameter2);
	}

	/**
	 * @see org.mmarini.fuzzy.IExpression#accept(org.mmarini.fuzzy.IExpressionVisitor)
	 */
	@Override
	public void accept(IExpressionVisitor visitor) {
		for (Iterator iter = this.getParameter().iterator(); iter.hasNext();) {
			((IExpression) iter.next()).accept(visitor);
		}
	}

	/**
	 * 
	 */
	public void addParameter(IExpression parameter) {
		this.getParameter().add(parameter);
	}

	/**
	 * Calculates the value
	 * 
	 * @param context
	 *            the context
	 * @return the value
	 */
	protected abstract FuzzyBoolean calculateValue(IEvaluateContext context);

	/**
	 * Copies the string value of parameters
	 * 
	 * @param value
	 *            the coping to string buffer
	 */
	protected void copyParmsToString(StringBuffer value) {
		List parms = this.getParameter();
		int size = parms.size();
		for (int i = 0; i < size; ++i) {
			if (i > 0)
				value.append(",");
			value.append(parms.get(i));
		}
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object arg0) {
		if (arg0 == this)
			return true;
		if (arg0 == null)
			return false;
		if (!(arg0 instanceof AbstractExpression))
			return false;
		return this.getParameter().equals(
				((AbstractExpression) arg0).getParameter());
	}

	/**
	 * @see org.mmarini.fuzzy.IExpression#evaluate(org.mmarini.fuzzy.IEvaluateContext)
	 */
	@Override
	public FuzzyBoolean evaluate(IEvaluateContext context) {
		FuzzyBoolean value = this.getValue();
		if (value == null) {
			value = this.calculateValue(context);
			this.setValue(value);
		}
		return value;
	}

	/**
	 * Returns the paramters
	 * 
	 * @return the parameters.
	 */
	public List getParameter() {
		return parameters;
	}

	/**
	 * Returns a paramter
	 * 
	 * @param index
	 *            the index of paramter
	 * @return the paramter
	 */
	public IExpression getParameter(int index) {
		return (IExpression) parameters.get(index);
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
		return this.getParameter().hashCode();
	}

	/**
	 * @see org.mmarini.fuzzy.IExpression#reset()
	 */
	@Override
	public void reset() {
		this.setValue(null);
		for (Iterator iter = this.getParameter().iterator(); iter.hasNext();) {
			((IExpression) iter.next()).reset();
		}
	}

	/**
	 * @see org.mmarini.fuzzy.IExpression#scan(org.mmarini.fuzzy.IWeightContext)
	 */
	@Override
	public void scan(IWeightContext context) {
		if (this.getValue().isKnown())
			return;
		for (Iterator iter = this.getParameter().iterator(); iter.hasNext();) {
			IExpression exp = (IExpression) iter.next();
			exp.scan(context);
		}
	}

	/**
	 * @param value
	 *            The value to set.
	 */
	protected void setValue(FuzzyBoolean value) {
		this.value = value;
	}
}