/**
 * 
 */
package org.mmarini.fuzzy;

import java.util.Set;

/**
 * @author US00852
 * 
 */
public class Rule implements Command {
	private Expression condition;
	private AssignListCmd thenConseguences;
	private AssignListCmd elseConseguences;

	/**
	 * 
	 * @param condition
	 * @param thenConseguences
	 * @param elseConseguences
	 */
	public Rule(Expression condition, AssignListCmd thenConseguences,
			AssignListCmd elseConseguences) {
		this.condition = condition;
		this.thenConseguences = thenConseguences;
		this.elseConseguences = elseConseguences;
	}

	/**
	 * 
	 */
	@Override
	public void execute(ExecutionContext ctx) {
		condition.execute(ctx);
		FuzzyBoolean value = ctx.pop();
		ctx.push(value);
		thenConseguences.execute(ctx);
		ctx.push(value.not());
		elseConseguences.execute(ctx);
	}

	/**
	 * @return the condition
	 */
	public Expression getCondition() {
		return condition;
	}

	/**
	 * @return the elseConseguences
	 */
	public AssignListCmd getElseConseguences() {
		return elseConseguences;
	}

	/**
	 * @return the thenConseguences
	 */
	public AssignListCmd getThenConseguences() {
		return thenConseguences;
	}

	/**
	 * 
	 * @param p
	 * @return
	 */
	public boolean hasConsequence(String p) {
		return thenConseguences.hasPredicate(p)
				|| elseConseguences.hasPredicate(p);
	}

	/**
	 * 
	 * @param inferences
	 */
	public Set<String> mapToCondition(Set<String> inferences) {
		return condition.mapToPredicate(inferences);
	}

	/**
	 * 
	 * @param inferences
	 * @return
	 */
	public Set<String> mapToInference(Set<String> inferences) {
		return thenConseguences.mapToPredicate(elseConseguences
				.mapToPredicate(inferences));
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("if ").append(condition).append(" then ")
				.append(thenConseguences).append(" else ")
				.append(elseConseguences);
		return builder.toString();
	}

}
