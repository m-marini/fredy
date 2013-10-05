package org.mmarini.fuzzy;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.hasItems;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.beans.HasPropertyWithValue.hasProperty;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import junit.framework.TestCase;

import org.junit.Before;
import org.junit.Test;

/**
 * @author US00852
 * @version $Id: AnalisysTest.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class AnalisysTest {
	static final FuzzyBoolean QUITE_TRUE = FuzzyBoolean.QUITE_TRUE;
	static final FuzzyBoolean UNKNOWN = FuzzyBoolean.UNKNOWN;
	static final FuzzyBoolean QUITE_FALSE = FuzzyBoolean.QUITE_FALSE;
	static final FuzzyBoolean FALSE = FuzzyBoolean.FALSE;
	static final FuzzyBoolean TRUE = FuzzyBoolean.TRUE;
	static final String PREDICATE_C = "C";
	static final String PREDICATE_B = "B";
	static final String PREDICATE_A = "A";
	static final String NO_PREDICATE = "?????????";

	Analisys analisys;
	Analisys analisys1;
	Analisys analisys2;
	IPredicate predicateA;
	IPredicate predicateB;
	IPredicate predicateC;
	Rule rule1;
	Rule rule2;
	FuzzyBoolean[][] values;
	private Weight[][] weights;
	private static final Weight W00 = Weight.NULL_WEIGHT;
	private static final Weight W10 = new Weight(1, 0);
	private static final Weight W11 = new Weight(1, 1);

	/**
	 * @see TestCase#setUp()
	 */
	@Before
	public void init() throws Exception {
		values = new FuzzyBoolean[][] { { FALSE, FALSE, UNKNOWN },
				{ FALSE, QUITE_FALSE, UNKNOWN }, { FALSE, UNKNOWN, UNKNOWN },
				{ FALSE, QUITE_TRUE, QUITE_TRUE }, { FALSE, TRUE, TRUE },

				{ QUITE_FALSE, QUITE_FALSE, UNKNOWN },
				{ QUITE_FALSE, UNKNOWN, UNKNOWN },
				{ QUITE_FALSE, QUITE_TRUE, QUITE_TRUE },
				{ QUITE_FALSE, TRUE, TRUE },

				{ UNKNOWN, UNKNOWN, UNKNOWN },
				{ UNKNOWN, QUITE_TRUE, QUITE_TRUE }, { UNKNOWN, TRUE, TRUE },

				{ QUITE_TRUE, QUITE_TRUE, QUITE_TRUE },
				{ QUITE_TRUE, TRUE, TRUE },

				{ TRUE, TRUE, TRUE }, };

		weights = new Weight[][] { { W00, W00 }, { W00, W00 }, { W00, W10 },
				{ W00, W00 }, { W00, W00 },

				{ W00, W00 }, { W00, W10 }, { W00, W00 }, { W00, W00 },

				{ W11, W11 }, { W10, W00 }, { W00, W00 },

				{ W00, W00 }, { W00, W00 },

				{ W00, W00 } };

		/*
		 * Creare una analisi iniziale contente 2 regole con 3 predicati. creare
		 * un insieme di valori da testare e verificare i risultati
		 */
		analisys1 = new Analisys();
		analisys2 = new Analisys();

		/*
		 * analisi: if (A) then (C) else () if (B) then (C) else ()
		 */
		analisys = new Analisys();
		predicateA = analisys.addPredicate(PREDICATE_A);
		predicateB = analisys.addPredicate(PREDICATE_B);
		predicateC = analisys.addPredicate(PREDICATE_C);

		rule1 = new Rule();
		rule1.setIfExpression(new PredicateExpression(predicateA));
		rule1.addThenExpression(new Assign(predicateC));

		rule2 = new Rule();
		rule2.setIfExpression(new PredicateExpression(predicateB));
		rule2.addThenExpression(new Assign(predicateC));

		analisys.addRule(rule1);
		analisys.addRule(rule2);
	}

	@Test
	public void testAnalize() {
		for (int i = 0; i < values.length; ++i) {
			String testName = "Test " + i + " " + values[i][0] + ","
					+ values[i][1];
			predicateA.setValue(values[i][0]);
			predicateB.setValue(values[i][1]);
			analisys.reset();
			analisys.analize();
			assertThat(testName, predicateC,
					hasProperty("value", equalTo(values[i][2])));
			assertThat(testName, predicateA,
					hasProperty("weight", equalTo(weights[i][0])));
			assertThat(testName, predicateB,
					hasProperty("weight", equalTo(weights[i][1])));

			testName = "Test " + i + " " + values[i][1] + "," + values[i][0];
			predicateA.setValue(values[i][1]);
			predicateB.setValue(values[i][0]);
			analisys.reset();
			analisys.analize();
			assertThat(testName, predicateC,
					hasProperty("value", equalTo(values[i][2])));
			assertThat(testName, predicateA,
					hasProperty("weight", equalTo(weights[i][1])));
			assertThat(testName, predicateB,
					hasProperty("weight", equalTo(weights[i][0])));
		}
	}

	/*
	 * Class under test for boolean equals(Object)
	 */

	@Test
	public void testEqualsObject1() {
		assertThat(analisys1.equals(analisys1), is(equalTo(true)));
	}

	@Test
	public void testEqualsObject2() {
		assertThat(analisys1.equals(null), is(equalTo(false)));
	}

	@Test
	public void testEqualsObject3() {
		assertThat(analisys1.equals(new Object()), is(equalTo(false)));
	}

	@Test
	public void testEqualsObject4() {
		assertThat(analisys1.equals(analisys2), is(equalTo(true)));
	}

	@Test
	public void testEqualsObject5() {
		assertThat(analisys2.equals(analisys1), is(equalTo(true)));
	}

	@Test
	public void testEqualsObject6() {
		assertThat(analisys1.hashCode() == analisys2.hashCode(),
				is(equalTo(true)));
	}

	@Test
	public void testEqualsObject8() {
		analisys1.addPredicate(PREDICATE_A);
		assertFalse(analisys1.equals(analisys2));
		assertFalse(analisys2.equals(analisys1));
	}

	@Test
	public void testEqualsObject9() {
		analisys1.addPredicate(PREDICATE_A);
		analisys2.addPredicate(PREDICATE_A);
		assertTrue(analisys1.equals(analisys2));
		assertTrue(analisys2.equals(analisys1));
		assertTrue(analisys1.hashCode() == analisys2.hashCode());
	}

	@Test
	public void testEqualsObject10() {
		analisys1.addRule(rule1);
		assertFalse(analisys1.equals(analisys2));
		assertFalse(analisys2.equals(analisys1));

	}

	@Test
	public void testEqualsObject7() {
		analisys1.addRule(rule1);
		analisys2.addRule(rule1);
		assertTrue(analisys1.equals(analisys2));
		assertTrue(analisys2.equals(analisys1));
		assertTrue(analisys1.hashCode() == analisys2.hashCode());
	}

	@Test
	public void testGetEvidence() {
		assertThat(analisys, hasProperty("evidence", hasItems(predicateC)));
	}

	@Test
	public void testGetHypotesys() {
		assertThat(analisys, hasProperty("hypotesys", hasItems(predicateC)));
	}

	@Test
	public void testGetPostulate() {
		assertThat(analisys,
				hasProperty("postulate", hasItems(predicateA, predicateB)));
	}

	@Test
	public void testGetPredicateString() {
		assertNull(analisys.getPredicate(NO_PREDICATE));
		assertEquals(predicateA, analisys.getPredicate(PREDICATE_A));
		assertEquals(predicateB, analisys.getPredicate(PREDICATE_B));
		assertEquals(predicateC, analisys.getPredicate(PREDICATE_C));
	}

}