package org.mmarini.fuzzy;

/**
 * @author US00852
 * @version $Id: IExpression.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public interface IExpression {

	/**
	 * Accepts a visitor
	 * 
	 * @param visitor
	 *            the visitor
	 */
	public abstract void accept(IExpressionVisitor visitor);

	/**
	 * Evaluates the expression
	 * 
	 * @return the value
	 */
	public abstract FuzzyBoolean evaluate(IEvaluateContext context);

	/**
	 * Get the value of expression
	 * 
	 * @return the value
	 */
	public abstract FuzzyBoolean getValue();

	/**
	 * Reset the state of expression
	 */
	public abstract void reset();

	/**
	 * Seeks for the postulates
	 * 
	 * @param context
	 *            the context
	 */
	public abstract void scan(IWeightContext context);
}