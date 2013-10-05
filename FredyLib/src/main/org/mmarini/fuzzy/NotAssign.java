package org.mmarini.fuzzy;

import java.io.Serializable;

/**
 * @author US00852
 * @version $Id: NotAssign.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class NotAssign implements IAssignExpression, Serializable {
	private static final long serialVersionUID = -6881577559259156467L;
	private IPredicate predicate;

	/**
	 * Creates a Not Assign Expression
	 * 
	 * @param predicate
	 *            the predicate
	 */
	public NotAssign(IPredicate predicate) {
		this.predicate = predicate;
	}

	/**
	 * @see org.mmarini.fuzzy.IAssignExpression#accept(org.mmarini.fuzzy.IAssignVisitor)
	 */
	@Override
	public void accept(IAssignVisitor visitor) {
		visitor.visitPredicate(this.getPredicate());
	}

	/**
	 * @see org.mmarini.fuzzy.IAssignExpression#assign(org.mmarini.fuzzy.FuzzyBoolean)
	 */
	@Override
	public void assign(FuzzyBoolean value) {
		IPredicate predicate = this.getPredicate();
		predicate.setValue(value.not().and(predicate.getValue()));
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object arg0) {
		if (arg0 == this)
			return true;
		if (arg0 == null)
			return false;
		if (!(arg0 instanceof NotAssign))
			return false;
		Object p1 = this.getPredicate();
		return p1 == null ? ((NotAssign) arg0).getPredicate() == null : p1
				.equals(((NotAssign) arg0).getPredicate());
	}

	/**
	 * @return Returns the predicate.
	 */
	public IPredicate getPredicate() {
		return predicate;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		Object pr = this.getPredicate();
		return pr == null ? 0 : pr.hashCode();
	}

	/**
	 * @see org.mmarini.fuzzy.IAssignExpression#isAssigner(org.mmarini.fuzzy.IPredicate)
	 */
	@Override
	public boolean isAssigner(IPredicate evidence) {
		if (this.getPredicate().getValue().isFalse())
			return false;
		if (!this.getPredicate().equals(evidence))
			return false;
		return true;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuffer bfr = new StringBuffer();
		bfr.append("not(");
		bfr.append(this.getPredicate().getName());
		bfr.append(")");
		return bfr.toString();
	}
}