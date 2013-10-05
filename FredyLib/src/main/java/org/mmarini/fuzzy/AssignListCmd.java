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
public class AssignListCmd extends ArrayList<AssignCmd> implements AssignCmd {
	private static final long serialVersionUID = -2409407796651355076L;

	/**
	 * 
	 * @param commands
	 */
	public AssignListCmd(AssignCmd... commands) {
		for (AssignCmd c : commands) {
			add(c);
		}
	}

	/**
	 * @see org.mmarini.fuzzy.Command#execute(org.mmarini.fuzzy.ExecutionContext)
	 */
	@Override
	public void execute(ExecutionContext ctx) {
		FuzzyBoolean value = ctx.pop();
		for (AssignCmd c : this) {
			ctx.push(value);
			c.execute(ctx);
		}
	}

	/**
	 * @see org.mmarini.fuzzy.AssignCmd#hasPredicate(java.lang.String)
	 */
	@Override
	public boolean hasPredicate(String predicate) {
		for (AssignCmd p : this) {
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
		for (AssignCmd p : this) {
			predicates = p.mapToPredicate(predicates);
		}
		return predicates;
	}
}
