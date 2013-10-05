package org.mmarini.fuzzy;

/**
 * @author US00852
 * @version $Id: IAssignExpression.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public interface IAssignExpression {

	/**
	 * Accepts a visitor
	 * 
	 * @param visitor
	 *            the visitor
	 */
	public abstract void accept(IAssignVisitor visitor);

	/**
	 * Assigns the value
	 * 
	 * @param value
	 *            the value
	 */
	public abstract void assign(FuzzyBoolean value);

	/**
	 * Calculates the weight
	 * 
	 * @param evidence
	 *            the evidence
	 * 
	 * @return the weight
	 */
	public abstract boolean isAssigner(IPredicate evidence);
}