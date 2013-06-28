package org.mmarini.fuzzy;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

/**
 * @author US00852
 * @version $Id: Rule.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class Rule implements Serializable {
	private boolean applied;
	private boolean weighted;
	private IExpression ifExpression;
	private List thenExpression = new ArrayList();
	private List elseExpression = new ArrayList();

	/**
	 * Accepts a visitor to a list of IAssignExpression
	 * 
	 * @param list
	 *            the list
	 * @param visitorthe
	 *            visitor
	 */
	protected void accept(List list, IAssignVisitor visitor) {
		for (Iterator iter = list.iterator(); iter.hasNext();) {
			((IAssignExpression) iter.next()).accept(visitor);
		}
	}

	/**
	 * Accepts a visitor to else list
	 * 
	 * @param visitor
	 *            the visitor
	 */
	public void acceptElse(IAssignVisitor visitor) {
		this.accept(this.getElseExpression(), visitor);
	}

	/**
	 * Accepts a visitor to then list
	 * 
	 * @param visitor
	 *            the visitor
	 */
	public void acceptThen(IAssignVisitor visitor) {
		this.accept(this.getThenExpression(), visitor);
	}

	/**
	 * Adds an else collection expressions
	 * 
	 * @param exp
	 *            the collection
	 */
	public void addElseExpression(Collection exp) {
		this.getElseExpression().addAll(exp);
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
	public void addThenExpression(Collection exp) {
		this.getThenExpression().addAll(exp);
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
	protected void appendListToString(StringBuffer bfr, List list) {
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
		if (this.isApplied())
			return;
		this.setApplied(true);
		FuzzyBoolean value = this.getIfExpression().evaluate(context);
		for (Iterator iter = this.getThenExpression().iterator(); iter
				.hasNext();) {
			((IAssignExpression) iter.next()).assign(value);
		}
		value = value.not();
		for (Iterator iter = this.getElseExpression().iterator(); iter
				.hasNext();) {
			((IAssignExpression) iter.next()).assign(value);
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
	public boolean equals(Object other) {
		if (this == other)
			return true;
		if (other == null)
			return false;
		if (!(other instanceof Rule))
			return false;
		Rule rule = (Rule) other;
		if (this.isApplied() != rule.isApplied())
			return false;
		Object obj = this.getIfExpression();
		if (obj == null) {
			if (rule.getIfExpression() != null)
				return false;
		} else if (!(obj.equals(rule.getIfExpression())))
			return false;
		if (!(this.getThenExpression().equals(rule.getThenExpression())))
			return false;
		if (!(this.getElseExpression().equals(rule.getElseExpression())))
			return false;
		return true;
	}

	/**
	 * @return Returns the elseExpression.
	 */
	protected List getElseExpression() {
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
	 * @return Returns the thenExpression.
	 */
	protected List getThenExpression() {
		return thenExpression;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		int hash = new Boolean(this.isApplied()).hashCode();
		Object obj = this.getIfExpression();
		if (obj != null)
			hash = hash * 31 + obj.hashCode();
		hash = hash * 31 + this.getThenExpression().hashCode();
		hash = hash * 31 + this.getElseExpression().hashCode();
		return hash;
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
		for (Iterator iter = this.getThenExpression().iterator(); iter
				.hasNext();) {
			IAssignExpression exp = (IAssignExpression) iter.next();
			if (exp.isAssigner(predicate))
				return true;
		}
		for (Iterator iter = this.getElseExpression().iterator(); iter
				.hasNext();) {
			IAssignExpression exp = (IAssignExpression) iter.next();
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
		this.setApplied(false);
		this.getIfExpression().reset();
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
	 * Sets true if the rule has been applied
	 * 
	 * @param applied
	 *            true if the rule has been applied
	 */
	protected void setApplied(boolean applied) {
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
		StringBuffer bfr = new StringBuffer();
		bfr.append("if(");
		bfr.append(this.getIfExpression());
		bfr.append(")then(");
		appendListToString(bfr, this.getThenExpression());
		bfr.append(")else(");
		appendListToString(bfr, this.getElseExpression());
		bfr.append(")");
		return bfr.toString();
	}
}