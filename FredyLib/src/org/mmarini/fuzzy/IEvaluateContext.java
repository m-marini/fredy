package org.mmarini.fuzzy;

/**
 * @author US00852
 * @version $Id: IEvaluateContext.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public interface IEvaluateContext {

	/**
	 * Evaluates a predicate
	 * 
	 * @param predicate
	 *            the predicate
	 * @return the value of predicate
	 */
	public abstract FuzzyBoolean evaluate(IPredicate predicate);
}