/**
 * 
 */
package org.mmarini.fuzzy;

import java.util.Set;

/**
 * @author US00852
 * 
 */
public class AssignFalseCmd implements AssignCmd {
	private String predicate;

	/**
	 * @param parameters
	 */
	public AssignFalseCmd(String predicate) {
		this.predicate = predicate;
	}

	/**
	 * @see org.mmarini.fuzzy.Command#execute(org.mmarini.fuzzy.ExecutionContext)
	 */
	@Override
	public void execute(ExecutionContext ctx) {
		ctx.assignFalse(predicate);
	}

	/**
	 * @return the predicate
	 */
	public String getPredicate() {
		return predicate;
	}

	/**
	 * @see org.mmarini.fuzzy.AssignCmd#hasPredicate(java.lang.String)
	 */
	@Override
	public boolean hasPredicate(String predicate) {
		return this.predicate.equals(predicate);
	}

	/**
	 * @see org.mmarini.fuzzy.Command#mapToPredicate(java.util.Set)
	 */
	@Override
	public Set<String> mapToPredicate(Set<String> predicates) {
		predicates.add(predicate);
		return predicates;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append(predicate).append("=false");
		return builder.toString();
	}
}
