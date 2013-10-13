/**
 * 
 */
package org.mmarini.fuzzy;

import java.util.Collection;
import java.util.Set;

import org.mmarini.functional.FSet;
import org.mmarini.functional.SetImpl;

/**
 * @author US00852
 * 
 */
public class Rule implements Command {
	private Expression condition;
	private AssertListCmd thenConseguences;
	private AssertListCmd elseConseguences;

	/**
	 * 
	 * @param condition
	 * @param thenConseguences
	 * @param elseConseguences
	 */
	public Rule(Expression condition, AssertListCmd thenConseguences,
			AssertListCmd elseConseguences) {
		this.condition = condition;
		this.thenConseguences = thenConseguences;
		this.elseConseguences = elseConseguences;
	}

	/**
	 * 
	 * @param relations
	 * @return
	 */
	public Collection<Relation> addRelations(Collection<Relation> relations) {
		if (thenConseguences != null) {
			for (String s : condition.mapToPredicate(null)) {
				for (String t : thenConseguences.mapToPredicate(null)) {
					relations.add(new Relation(s, t));
				}
			}
		}
		return relations;
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
	public AssertListCmd getElseConseguences() {
		return elseConseguences;
	}

	/**
	 * @return the thenConseguences
	 */
	public AssertListCmd getThenConseguences() {
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
	 * @return
	 */
	public FSet<String> retrieveCondition() {
		return condition.mapToPredicate(new SetImpl<String>());
	}

	/**
	 * 
	 * @param inferences
	 * @return
	 */
	public Set<String> retrieveInference() {
		FSet<String> inferences = new SetImpl<String>();
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
}
