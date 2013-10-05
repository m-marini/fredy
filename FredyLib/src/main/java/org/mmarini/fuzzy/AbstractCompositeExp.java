/**
 * 
 */
package org.mmarini.fuzzy;

import java.util.ArrayList;
import java.util.Set;

/**
 * @author US00852
 * 
 */
public abstract class AbstractCompositeExp extends ArrayList<Expression>
		implements Expression {
	private static final long serialVersionUID = -858890432738270881L;

	/**
	 * 
	 */
	protected AbstractCompositeExp(Expression... parameters) {
		for (Expression p : parameters) {
			add(p);
		}
	}

	/**
	 * @see org.mmarini.fuzzy.Expression#hasPredicate(java.lang.String)
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
	 * @see org.mmarini.fuzzy.Command#mapToPredicate(java.util.Set)
	 */
	@Override
	public Set<String> mapToPredicate(Set<String> predicates) {
		for (Expression p : this) {
			predicates = p.mapToPredicate(predicates);
		}
		return predicates;
	}
}
