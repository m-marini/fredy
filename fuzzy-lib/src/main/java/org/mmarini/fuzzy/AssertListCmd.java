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
public class AssertListCmd extends ArrayList<AssertCmd> implements AssertCmd {
	private static final long serialVersionUID = -2409407796651355076L;

	/**
	 * 
	 * @param commands
	 */
	public AssertListCmd(AssertCmd... commands) {
		for (AssertCmd c : commands) {
			add(c);
		}
	}

	/**
	 * @see org.mmarini.fuzzy.Command#execute(org.mmarini.fuzzy.ExecutionContext)
	 */
	@Override
	public void execute(ExecutionContext ctx) {
		FuzzyBoolean value = ctx.pop();
		for (AssertCmd c : this) {
			ctx.push(value);
			c.execute(ctx);
		}
	}

	/**
	 * @see org.mmarini.fuzzy.PredicateContainer#hasPredicate(java.lang.String)
	 */
	@Override
	public boolean hasPredicate(String predicate) {
		for (AssertCmd p : this) {
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
		for (AssertCmd p : this) {
			predicates = p.mapToPredicate(predicates);
		}
		return predicates;
	}
}
