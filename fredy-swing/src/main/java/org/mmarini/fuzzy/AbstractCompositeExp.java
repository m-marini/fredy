/**
 * 
 */
package org.mmarini.fuzzy;

import java.util.ArrayList;

import org.mmarini.functional.FSet;

/**
 * @author US00852
 * 
 */
public abstract class AbstractCompositeExp extends ArrayList<Expression>
		implements Expression {
	private static final long serialVersionUID = -858890432738270881L;

	/**
	 * 
	 * @param parameters
	 */
	protected AbstractCompositeExp(Expression... parameters) {
		for (Expression p : parameters) {
			add(p);
		}
	}

	/**
	 * @see org.mmarini.fuzzy.PredicateContainer#hasPredicate(java.lang.String)
	 */
	@Override
	public boolean hasPredicate(String predicate) {
		for (Expression p : this) {
			if (p.hasPredicate(predicate))
				return true;
		}
		return false;
	}

	/**
	 * @see org.mmarini.fuzzy.PredicateContainer#mapToPredicate(org.mmarini.functional
	 *      .FSet)
	 */
	@Override
	public FSet<String> mapToPredicate(FSet<String> predicates) {
		for (Expression p : this) {
			predicates = p.mapToPredicate(predicates);
		}
		return predicates;
	}
}
