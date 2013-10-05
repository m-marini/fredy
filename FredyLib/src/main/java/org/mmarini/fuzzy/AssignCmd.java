/**
 * 
 */
package org.mmarini.fuzzy;

import java.util.Set;

/**
 * @author US00852
 * 
 */
public interface AssignCmd extends Command {

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
	public abstract Set<String> mapToPredicate(Set<String> predicates);

}
