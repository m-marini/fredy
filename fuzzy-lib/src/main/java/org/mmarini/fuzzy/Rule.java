/**
 * 
 */
package org.mmarini.fuzzy;

import java.util.Collection;
import java.util.HashSet;
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
		if (thenConseguences != null) {
			ctx.push(value);
			thenConseguences.execute(ctx);
		}
		if (elseConseguences != null) {
			ctx.push(value.not());
			elseConseguences.execute(ctx);
		}
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
		return (thenConseguences != null && thenConseguences.hasPredicate(p))
				|| (elseConseguences != null && elseConseguences
						.hasPredicate(p));
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
		if (thenConseguences != null) {
			inferences = thenConseguences.mapToPredicate(inferences);
		}
		if (elseConseguences != null) {
			inferences = elseConseguences.mapToPredicate(inferences);
		}
		return inferences;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("if ").append(condition);
		if (thenConseguences != null) {
			builder.append(" then ").append(thenConseguences);
		}
		if (thenConseguences != null) {
			builder.append(" else ").append(elseConseguences);
		}
		return builder.toString();
	}

	/**
	 * 
	 * @param relations
	 * @return
	 */
	public Collection<Relation> addRelations(Collection<Relation> relations) {
		if (thenConseguences != null) {
			for (String s : condition.mapToPredicate(new HashSet<String>())) {
				for (String t : thenConseguences
						.mapToPredicate(new HashSet<String>())) {
					relations.add(new Relation(s, t));
				}
			}
		}
		return relations;
	}
}
