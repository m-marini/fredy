package org.mmarini.fuzzy;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static org.hamcrest.collection.IsMapContaining.hasKey;
import static org.junit.Assert.assertThat;

import java.util.Set;

import org.junit.Before;
import org.junit.Test;

/**
 * @author US00852
 * @version $Id: WeightCalculatorTest.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class WeightCalculatorTest {
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
	@Before
	public void init() throws Exception {
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

	/**
	 * 
	 */
	@Test
	public void testHypDependencyAsValue() {
		assertThat(calc.getHypDependecy(), is(notNullValue()));
	}

	@Test
	public void testHypDependencyHasKeyA() {
		calc.startHypotesys(hypotesysA);
		calc.addPostulate(predicateA);
		calc.addPostulate(predicateB);
		calc.addPostulate(predicateA);
		calc.addPostulate(predicateC);
		assertThat(calc.getHypDependecy(), hasKey(hypotesysA));
	}

	@Test
	public void testHypDependencyHasKeyB() {
		calc.startHypotesys(hypotesysA);
		calc.addPostulate(predicateA);
		calc.addPostulate(predicateB);
		calc.addPostulate(predicateA);
		calc.addPostulate(predicateC);
		assertThat(calc.getHypDependecy(), hasKey(hypotesysB));
	}

	@Test
	public void testHypDependencyHasSize3() {
		calc.startHypotesys(hypotesysA);
		calc.addPostulate(predicateA);
		calc.addPostulate(predicateB);
		calc.addPostulate(predicateA);
		calc.addPostulate(predicateC);
		assertThat(calc.getHypDependecy().size(), is(equalTo(3)));
	}

	@Test
	public void testHypDependency4() {
		calc.startHypotesys(hypotesysA);
		calc.addPostulate(predicateA);
		calc.addPostulate(predicateB);
		calc.addPostulate(predicateA);
		calc.addPostulate(predicateC);

		calc.startHypotesys(hypotesysB);
		calc.addPostulate(predicateA);
		calc.addPostulate(predicateB);
		calc.addPostulate(predicateA);

		Set<IPredicate> set = calc.getHypDependecy().get(hypotesysA);

		assertThat(set, is(notNullValue()));
		assertThat(set, hasSize(3));

		set = calc.getHypDependecy().get(hypotesysB);
		assertThat(set, is(notNullValue()));
		assertThat(set, hasSize(2));

		assertThat(calc.getWeight(predicateA), is(equalTo(W22)));
		assertThat(calc.getWeight(predicateB), is(equalTo(W22)));
		assertThat(calc.getWeight(predicateC), is(equalTo(W12)));
		assertThat(calc.getWeight(predicateD), is(equalTo(Weight.NULL_WEIGHT)));
	}
}