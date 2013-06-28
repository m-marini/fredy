package org.mmarini.fuzzy;

/**
 * @author US00852
 * @version $Id: IWeightContext.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public interface IWeightContext extends IScanContext {

	/**
	 * Sets the postulate
	 * 
	 * @param postulate
	 *            the predicate
	 */
	public abstract void addPostulate(IPredicate postulate);
}