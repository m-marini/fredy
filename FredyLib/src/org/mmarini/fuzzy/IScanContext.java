package org.mmarini.fuzzy;

/**
 * @author US00852
 * @version $Id: IScanContext.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public interface IScanContext {
	/**
	 * @param predicate
	 */
	public abstract void scan(IWeightContext context, IPredicate predicate);
}