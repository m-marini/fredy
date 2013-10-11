/**
 * 
 */
package org.mmarini.fuzzy;

import org.mmarini.functional.FSet;

/**
 * @author US00852
 * 
 */
public abstract class AbstractUnaryExp implements Expression {

	private Expression parameter;

	/**
	 * 
	 */
	protected AbstractUnaryExp(Expression parameter) {
		this.parameter = parameter;
	}

	/**
	 * @return the parameter
	 */
	protected Expression getParameter() {
		return parameter;
	}

	/**
	 * @see org.mmarini.fuzzy.PredicateContainer#hasPredicate(java.lang.String)
	 */
	@Override
	public boolean hasPredicate(String predicate) {
		return parameter.hasPredicate(predicate);
	}

	/**
	 * @see org.mmarini.fuzzy.PredicateContainer#mapToPredicate(org.mmarini.functional
	 *      .FSet)
	 */
	@Override
	public FSet<String> mapToPredicate(FSet<String> predicates) {
		return parameter.mapToPredicate(predicates);
	}
}
