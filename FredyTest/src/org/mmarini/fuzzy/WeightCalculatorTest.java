package org.mmarini.fuzzy;

import java.util.Map;
import java.util.Set;

import org.mmarini.fuzzy.IPredicate;
import org.mmarini.fuzzy.IScanContext;
import org.mmarini.fuzzy.IWeightContext;
import org.mmarini.fuzzy.Predicate;
import org.mmarini.fuzzy.Weight;
import org.mmarini.fuzzy.WeightCalculator;

import junit.framework.TestCase;

/**
 * @author US00852
 * @version $Id: WeightCalculatorTest.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class WeightCalculatorTest extends TestCase {
	private static final Weight W12 = new Weight(1, 2);
	private static final Weight W22 = new Weight(2, 2);
	private WeightCalculator calc;
	private IPredicate hypotesysA;
	private IPredicate hypotesysB;
	private IPredicate predicateA;
	private IPredicate predicateB;
	private IPredicate predicateC;
	private IPredicate predicateD;

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		hypotesysA = new Predicate("HA");
		hypotesysB = new Predicate("HB");
		predicateA = new Predicate("A");
		predicateB = new Predicate("B");
		predicateC = new Predicate("C");
		predicateD = new Predicate("D");
		calc = new WeightCalculator(new IScanContext() {

			@Override
			public void scan(IWeightContext context, IPredicate predicate) {
			}

		});
	}

	public void testAddPostulate() {
		Map map = calc.getHypDependecy();
		assertNotNull(map);
		Set set = (Set) calc.getHypDependecy().get(hypotesysA);
		assertNull(set);
		set = (Set) calc.getHypDependecy().get(hypotesysB);
		assertNull(set);

		calc.startHypotesys(hypotesysA);
		calc.addPostulate(predicateA);
		calc.addPostulate(predicateB);
		calc.addPostulate(predicateA);
		calc.addPostulate(predicateC);
		set = (Set) calc.getHypDependecy().get(hypotesysB);
		assertNull(set);
		set = (Set) calc.getHypDependecy().get(hypotesysA);
		assertNotNull(set);
		assertEquals(3, set.size());

		calc.startHypotesys(hypotesysB);
		calc.addPostulate(predicateA);
		calc.addPostulate(predicateB);
		calc.addPostulate(predicateA);
		set = (Set) calc.getHypDependecy().get(hypotesysA);
		assertNotNull(set);
		assertEquals(3, set.size());
		set = (Set) calc.getHypDependecy().get(hypotesysB);
		assertNotNull(set);
		assertEquals(2, set.size());

		assertEquals(W22, calc.getWeight(predicateA));
		assertEquals(W22, calc.getWeight(predicateB));
		assertEquals(W12, calc.getWeight(predicateC));
		assertEquals(Weight.NULL_WEIGHT, calc.getWeight(predicateD));
	}
}