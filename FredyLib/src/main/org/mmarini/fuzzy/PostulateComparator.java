package org.mmarini.fuzzy;

import java.util.Comparator;

/**
 * @author US00852
 * @version $Id: PostulateComparator.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class PostulateComparator implements Comparator<IPredicate> {
	private static PostulateComparator instance = new PostulateComparator();

	/**
	 * @return Returns the instance.
	 */
	public static PostulateComparator getInstance() {
		return instance;
	}

	/**
	 * 
	 */
	protected PostulateComparator() {
	}

	/** 
	 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
	 */
	@Override
	public int compare(IPredicate predicate1, IPredicate predicate2) {

		FuzzyBoolean val1 = predicate1.getValue();
		FuzzyBoolean val2 = predicate2.getValue();
		int diff = val1.known().compareTo(val2.known());
		if (diff != 0)
			return diff;

		if (FuzzyBoolean.UNKNOWN.equals(val1)) {
			diff = predicate1.getWeight().compareTo(predicate2.getWeight());
			if (diff != 0)
				return -diff;
		} else {
			diff = val1.compareTo(val2);
			if (diff != 0)
				return -diff;
		}
		return predicate1.getName().compareToIgnoreCase(predicate2.getName());
	}

}