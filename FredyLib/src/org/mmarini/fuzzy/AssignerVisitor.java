package org.mmarini.fuzzy;

/**
 * @author US00852
 * @version $Id: AssignerVisitor.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class AssignerVisitor implements IAssignVisitor {
	private boolean assigner = false;
	private IPredicate predicate;

	/**
	 * @param predicate
	 */
	public AssignerVisitor(IPredicate predicate) {
		this.predicate = predicate;
	}

	/**
	 * @return Returns the predicate.
	 */
	protected IPredicate getPredicate() {
		return predicate;
	}

	/**
	 * @return Returns the assigner.
	 */
	public boolean isAssigner() {
		return assigner;
	}

	/**
	 * @see org.mmarini.fuzzy.IAssignVisitor#visitPredicate(org.mmarini.fuzzy.IPredicate)
	 */
	@Override
	public void visitPredicate(IPredicate predicate) {
		assigner = assigner || predicate == this.getPredicate();
	}
}