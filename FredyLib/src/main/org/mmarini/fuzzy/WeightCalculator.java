package org.mmarini.fuzzy;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * @author US00852
 * @version $Id: WeightCalculator.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class WeightCalculator implements IWeightContext {
	private IScanContext seeker;

	private Map<IPredicate, Set<IPredicate>> hypDependecy;
	private Set<IPredicate> postulates;

	/**
	 * @param analisys
	 */
	public WeightCalculator(IScanContext seeker) {
		hypDependecy = new HashMap<IPredicate, Set<IPredicate>>();
		this.seeker = seeker;
	}

	/**
	 * @see org.mmarini.fuzzy.IWeightContext#addPostulate(org.mmarini.fuzzy.IPredicate)
	 */
	@Override
	public void addPostulate(IPredicate postulate) {
		postulates.add(postulate);
	}

	/**
	 * @param postulate
	 * @return
	 */
	public Weight getWeight(IPredicate postulate) {
		Set<IPredicate> union = new HashSet<IPredicate>();
		int counter = 0;
		for (Set<IPredicate> set : hypDependecy.values()) {
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
	 * 
	 * @return
	 */
	IScanContext getSeeker() {
		return seeker;
	}

	/**
	 * 
	 * @return
	 */
	Map<IPredicate, Set<IPredicate>> getHypDependecy() {
		return hypDependecy;
	}

	/**
	 * 
	 * @return
	 */
	Set<IPredicate> getPostulates() {
		return postulates;
	}

	/**
	 * @see org.mmarini.fuzzy.IWeightContext#seekForPostulate(org.mmarini.fuzzy.IPredicate)
	 */
	@Override
	public void scan(IWeightContext context, IPredicate predicate) {
		seeker.scan(this, predicate);
	}

	/**
	 * @see org.mmarini.fuzzy.IWeightContext#startHypotesys(org.mmarini.fuzzy.IPredicate)
	 */
	public void startHypotesys(IPredicate hypotesys) {
		postulates = new HashSet<IPredicate>();
		hypDependecy.put(hypotesys, postulates);
	}
}