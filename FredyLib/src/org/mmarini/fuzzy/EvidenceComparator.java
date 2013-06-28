package org.mmarini.fuzzy;

import java.util.Comparator;

/**
 * @author US00852
 * @version $Id: EvidenceComparator.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class EvidenceComparator implements Comparator {
	private static EvidenceComparator instance = new EvidenceComparator();

	/**
	 * @return Returns the instance.
	 */
	public static EvidenceComparator getInstance() {
		return instance;
	}

	/**
	 * 
	 */
	protected EvidenceComparator() {
	}

	/**
	 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
	 */
	@Override
	public int compare(Object obj1, Object obj2) {
		IPredicate predicate1 = (IPredicate) obj1;
		IPredicate predicate2 = (IPredicate) obj2;
		FuzzyBoolean val1 = predicate1.getValue();
		FuzzyBoolean val2 = predicate2.getValue();
		int diff = val1.known().compareTo(val2.known());
		if (diff != 0)
			return -diff;
		diff = val1.compareTo(val2);
		if (diff != 0)
			return -diff;
		return predicate1.getName().compareToIgnoreCase(predicate2.getName());
	}
}