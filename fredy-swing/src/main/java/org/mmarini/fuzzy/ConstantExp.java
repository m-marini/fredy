/**
 * 
 */
package org.mmarini.fuzzy;

import org.mmarini.functional.FSet;

/**
 * @author US00852
 * 
 */
public class ConstantExp implements Expression {
	private FuzzyBoolean value;

	/**
	 * 
	 * @param value
	 */
	public ConstantExp(FuzzyBoolean value) {
		super();
		this.value = value;
	}

	/**
	 * @see org.mmarini.fuzzy.Command#execute(org.mmarini.fuzzy.ExecutionContext
	 *      )
	 */
	@Override
	public void execute(ExecutionContext ctx) {
		ctx.push(value);
	}

	/**
	 * 
	 * @return
	 */
	public FuzzyBoolean getValue() {
		return value;
	}

	/**
	 * @see org.mmarini.fuzzy.PredicateContainer#hasPredicate(java.lang.String)
	 */
	@Override
	public boolean hasPredicate(String predicate) {
		return false;
	}

	/**
	 * @see org.mmarini.fuzzy.PredicateContainer#mapToPredicate(org.mmarini.functional
	 *      .FSet)
	 */
	@Override
	public FSet<String> mapToPredicate(FSet<String> predicates) {
		return predicates;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return String.valueOf(value);
	}
}
