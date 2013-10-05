package org.mmarini.fuzzy;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * @author US00852
 * @version $Id: Rule.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class Rule implements Serializable {
	private static final long serialVersionUID = 4694488392171951860L;

	private boolean applied;
	private boolean weighted;
	private IExpression ifExpression;
	private List<IAssignExpression> thenExpression = new ArrayList<IAssignExpression>();
	private List<IAssignExpression> elseExpression = new ArrayList<IAssignExpression>();

	/**
	 * Accepts a visitor to a list of IAssignExpression
	 * 
	 * @param list
	 *            the list
	 * @param visitorthe
	 *            visitor
	 */
	private void accept(List<IAssignExpression> list, IAssignVisitor visitor) {
		for (IAssignExpression iter : list) {
			iter.accept(visitor);
		}
	}

	/**
	 * Accepts a visitor to else list
	 * 
	 * @param visitor
	 *            the visitor
	 */
	public void acceptElse(IAssignVisitor visitor) {
		accept(elseExpression, visitor);
	}

	/**
	 * Accepts a visitor to then list
	 * 
	 * @param visitor
	 *            the visitor
	 */
	public void acceptThen(IAssignVisitor visitor) {
		accept(thenExpression, visitor);
	}

	/**
	 * Adds an else collection expressions
	 * 
	 * @param exp
	 *            the collection
	 */
	public void addElseExpression(Collection<IAssignExpression> exp) {
		elseExpression.addAll(exp);
	}

	/**
	 * Adds an else expression
	 * 
	 * @param expression
	 *            the expression
	 */
	public void addElseExpression(IAssignExpression expression) {
		elseExpression.add(expression);
	}

	/**
	 * Adds a then collection expressions
	 * 
	 * @param exp
	 *            the collection
	 */
	public void addThenExpression(Collection<IAssignExpression> exp) {
		elseExpression.addAll(exp);
	}

	/**
	 * Adds a then expression
	 * 
	 * @param expression
	 *            the expression
	 */
	public void addThenExpression(IAssignExpression expression) {
		thenExpression.add(expression);
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	private void appendListToString(StringBuilder bfr,
			List<IAssignExpression> list) {
		for (int i = 0; i < list.size(); ++i) {
			if (i > 0)
				bfr.append(",");
			bfr.append(list.get(i));
		}
	}

	/**
	 * Applies the rule
	 */
	public void apply(IEvaluateContext context) {
		if (!applied) {
			applied = true;
			FuzzyBoolean value = ifExpression.evaluate(context);
			for (IAssignExpression iter : thenExpression) {
				iter.assign(value);
			}
			value = value.not();
			for (IAssignExpression iter : elseExpression) {
				iter.assign(value);
			}
		}
	}

	/**
	 * Return true if the rule assigns a predicate
	 * 
	 * @param predicate
	 *            the predicate
	 * @return true if the rule assigns the predicate
	 */
	public boolean assigns(IPredicate predicate) {
		AssignerVisitor visitor = new AssignerVisitor(predicate);
		this.acceptThen(visitor);
		if (visitor.isAssigner())
			return true;
		this.acceptElse(visitor);
		return visitor.isAssigner();
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Rule other = (Rule) obj;
		if (applied != other.applied)
			return false;
		if (elseExpression == null) {
			if (other.elseExpression != null)
				return false;
		} else if (!elseExpression.equals(other.elseExpression))
			return false;
		if (ifExpression == null) {
			if (other.ifExpression != null)
				return false;
		} else if (!ifExpression.equals(other.ifExpression))
			return false;
		if (thenExpression == null) {
			if (other.thenExpression != null)
				return false;
		} else if (!thenExpression.equals(other.thenExpression))
			return false;
		if (weighted != other.weighted)
			return false;
		return true;
	}

	/**
	 * 
	 * @return
	 */
	public List<IAssignExpression> getElseExpression() {
		return elseExpression;
	}

	/**
	 * Returns the ifExpression.
	 * 
	 * @return the ifExpression.
	 */
	public IExpression getIfExpression() {
		return ifExpression;
	}

	/**
	 * 
	 * @return
	 */
	public List<IAssignExpression> getThenExpression() {
		return thenExpression;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (applied ? 1231 : 1237);
		result = prime * result
				+ ((elseExpression == null) ? 0 : elseExpression.hashCode());
		result = prime * result
				+ ((ifExpression == null) ? 0 : ifExpression.hashCode());
		result = prime * result
				+ ((thenExpression == null) ? 0 : thenExpression.hashCode());
		result = prime * result + (weighted ? 1231 : 1237);
		return result;
	}

	/**
	 * Returns true if the rule has been applied
	 * 
	 * @return true if the rule has been applied
	 */
	public boolean isApplied() {
		return applied;
	}

	/**
	 * Returns true if the rule assign a predicate
	 * 
	 * @param predicate
	 *            the predicate
	 * @return true if the rule assign a predicate
	 */
	protected boolean isAssigner(IPredicate predicate) {
		for (IAssignExpression exp : thenExpression) {
			if (exp.isAssigner(predicate))
				return true;
		}
		for (IAssignExpression exp : elseExpression) {
			if (exp.isAssigner(predicate))
				return true;
		}
		return false;
	}

	/**
	 * @return Returns the weighted.
	 */
	public boolean isWeighted() {
		return weighted;
	}

	/**
	 * Resets the rule state
	 */
	public void reset() {
		applied = false;
		ifExpression.reset();
	}

	/**
	 * Resets the rule state
	 */
	public void resetWeighted() {
		this.setWeighted(false);
	}

	/**
	 * @param context
	 * @param evidence
	 */
	public void seekForPostulate(IWeightContext context, IPredicate evidence) {
		if (this.isWeighted())
			return;
		if (!this.isAssigner(evidence))
			return;
		this.setWeighted(true);
		this.getIfExpression().scan(context);
	}

	/**
	 * 
	 * @param applied
	 */
	public void setApplied(boolean applied) {
		this.applied = applied;
	}

	/**
	 * Sets the ifExpression
	 * 
	 * @param ifExpression
	 *            the ifExpression to set.
	 */
	public void setIfExpression(IExpression ifExpression) {
		this.ifExpression = ifExpression;
	}

	/**
	 * @param weighted
	 *            The weighted to set.
	 */
	public void setWeighted(boolean weighted) {
		this.weighted = weighted;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder bfr = new StringBuilder();
		bfr.append("if(");
		bfr.append(this.getIfExpression());
		bfr.append(")then(");
		appendListToString(bfr, thenExpression);
		bfr.append(")else(");
		appendListToString(bfr, thenExpression);
		bfr.append(")");
		return bfr.toString();
	}
}