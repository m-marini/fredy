/**
 * 
 */
package org.mmarini.fuzzy;

import java.util.Set;

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
	 * @see org.mmarini.fuzzy.Expression#hasPredicate(java.lang.String)
	 */
	@Override
	public boolean hasPredicate(String predicate) {
		return parameter.hasPredicate(predicate);
	}

	/**
	 * @see org.mmarini.fuzzy.Command#mapToPredicate(java.util.Set)
	 */
	@Override
	public Set<String> mapToPredicate(Set<String> predicates) {
		return parameter.mapToPredicate(predicates);
	}
}
