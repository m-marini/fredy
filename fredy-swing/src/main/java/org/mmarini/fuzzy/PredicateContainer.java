/**
 * 
 */
package org.mmarini.fuzzy;

import org.mmarini.functional.FSet;

/**
 * @author US00852
 * 
 */
public interface PredicateContainer {

	/**
	 * 
	 * @param predicate
	 * @return
	 */
	public abstract boolean hasPredicate(String predicate);

	/**
	 * 
	 * @param predicates
	 */
	public abstract FSet<String> mapToPredicate(FSet<String> predicates);
}
