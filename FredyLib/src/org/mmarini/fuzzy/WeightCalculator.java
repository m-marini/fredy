package org.mmarini.fuzzy;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * @author US00852
 * @version $Id: WeightCalculator.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class WeightCalculator implements IWeightContext {
	private IScanContext seeker;

	private Map hypDependecy = new HashMap();
	private Set postulates;

	/**
	 * @param analisys
	 */
	public WeightCalculator(IScanContext seeker) {
		this.seeker = seeker;
	}

	/**
	 * @see org.mmarini.fuzzy.IWeightContext#addPostulate(org.mmarini.fuzzy.IPredicate)
	 */
	@Override
	public void addPostulate(IPredicate postulate) {
		this.getPostulates().add(postulate);
	}

	/**
	 * Returns the weight map
	 * 
	 * @return Returns the weight map
	 */
	protected Map getHypDependecy() {
		return hypDependecy;
	}

	/**
	 * @return Returns the postulates.
	 */
	protected Set getPostulates() {
		return postulates;
	}

	protected IScanContext getSeeker() {
		return seeker;
	}

	/**
	 * @param postulate
	 * @return
	 */
	public Weight getWeight(IPredicate postulate) {
		Map map = this.getHypDependecy();
		Set union = new HashSet();
		int counter = 0;
		for (Iterator iter = map.values().iterator(); iter.hasNext();) {
			Set set = (Set) iter.next();
			if (set.contains(postulate)) {
				++counter;
				union.addAll(set);
			}
		}
		/*
		 * Calculate the number of dependecy minus 1 for itself
		 */
		int dep = union.size();
		if (dep > 0)
			--dep;
		return new Weight(counter, dep);
	}

	/**
	 * @see org.mmarini.fuzzy.IWeightContext#seekForPostulate(org.mmarini.fuzzy.IPredicate)
	 */
	@Override
	public void scan(IWeightContext context, IPredicate predicate) {
		this.getSeeker().scan(this, predicate);
	}

	/**
	 * @param postulates
	 *            The postulates to set.
	 */
	protected void setPostulates(Set postulates) {
		this.postulates = postulates;
	}

	/**
	 * @see org.mmarini.fuzzy.IWeightContext#startHypotesys(org.mmarini.fuzzy.IPredicate)
	 */
	public void startHypotesys(IPredicate hypotesys) {
		Set postulates = new HashSet();
		this.getHypDependecy().put(hypotesys, postulates);
		this.setPostulates(postulates);
	}
}