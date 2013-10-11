/**
 * 
 */
package org.mmarini.fuzzy;

import org.mmarini.functional.FSet;
import org.mmarini.functional.SetImpl;

/**
 * @author US00852
 * 
 */
public abstract class AbstractAssertCmd implements AssertCmd {
	private String predicate;

	/**
	 * @param parameters
	 */
	public AbstractAssertCmd(String predicate) {
		this.predicate = predicate;
	}

	/**
	 * @return the predicate
	 */
	public String getPredicate() {
		return predicate;
	}

	/**
	 * @see org.mmarini.fuzzy.AssertCmd#hasPredicate(java.lang.String)
	 */
	@Override
	public boolean hasPredicate(String predicate) {
		return this.predicate.equals(predicate);
	}

	/**
	 * @see org.mmarini.fuzzy.Command#mapToPredicate(java.util.Set)
	 */
	@Override
	public FSet<String> mapToPredicate(FSet<String> predicates) {
		if (predicates == null)
			predicates = new SetImpl<String>();
		predicates.add(predicate);
		return predicates;
	}
}
