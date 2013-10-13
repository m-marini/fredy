/**
 * 
 */
package org.mmarini.fuzzy;

import org.mmarini.functional.FSet;
import org.mmarini.functional.SetImpl;

/**
 * @author US00852
 * 
 */
public class PredicateExp implements Expression {
	private String predicate;

	/**
	 * 
	 * @param predicate
	 */
	public PredicateExp(String predicate) {
		this.predicate = predicate;
	}

	/**
	 * @see org.mmarini.fuzzy.Command#execute(org.mmarini.fuzzy.ExecutionContext
	 *      )
	 */
	@Override
	public void execute(ExecutionContext ctx) {
		ctx.push(predicate);
	}

	/**
	 * @return the predicate
	 */
	public String getPredicate() {
		return predicate;
	}

	/**
	 * @see org.mmarini.fuzzy.PredicateContainer#hasPredicate(java.lang.String)
	 */
	@Override
	public boolean hasPredicate(String p) {
		return predicate.equals(p);
	}

	/**
	 * @see org.mmarini.fuzzy.PredicateContainer#mapToPredicate(org.mmarini.functional
	 *      .FSet)
	 */
	@Override
	public FSet<String> mapToPredicate(FSet<String> predicates) {
		if (predicates == null)
			predicates = new SetImpl<String>();
		predicates.add(predicate);
		return predicates;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return String.valueOf(predicate);
	}

}
