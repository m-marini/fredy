package org.mmarini.fuzzy;

import java.util.List;
import java.util.Set;

import org.mmarini.fuzzy.Analisys;
import org.mmarini.fuzzy.Assign;
import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.IPredicate;
import org.mmarini.fuzzy.PredicateExpression;
import org.mmarini.fuzzy.Rule;
import org.mmarini.fuzzy.Weight;

import junit.framework.TestCase;

/**
 * @author US00852
 * @version $Id: AnalisysTest.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class AnalisysTest extends TestCase {
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
	@Override
	protected void setUp() throws Exception {
		super.setUp();
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

	public void testAnalize() {
		for (int i = 0; i < values.length; ++i) {
			String testName = "Test " + i + " " + values[i][0] + ","
					+ values[i][1];
			predicateA.setValue(values[i][0]);
			predicateB.setValue(values[i][1]);
			analisys.reset();
			analisys.analize();
			assertEquals(testName, values[i][2], predicateC.getValue());

			assertEquals(testName, weights[i][0], predicateA.getWeight());
			assertEquals(testName, weights[i][1], predicateB.getWeight());

			testName = "Test " + i + " " + values[i][1] + "," + values[i][0];
			predicateA.setValue(values[i][1]);
			predicateB.setValue(values[i][0]);
			analisys.reset();
			analisys.analize();
			assertEquals(testName, values[i][2], predicateC.getValue());

			assertEquals(testName, weights[i][1], predicateA.getWeight());
			assertEquals(testName, weights[i][0], predicateB.getWeight());
		}
	} /*
	 * Class under test for boolean equals(Object)
	 */

	public void testEqualsObject() {
		assertTrue(analisys1.equals(analisys1));
		assertFalse(analisys1.equals(null));
		assertFalse(analisys1.equals(new Object()));

		assertTrue(analisys1.equals(analisys2));
		assertTrue(analisys2.equals(analisys1));
		assertTrue(analisys1.hashCode() == analisys2.hashCode());

		analisys1.addPredicate(PREDICATE_A);
		assertFalse(analisys1.equals(analisys2));
		assertFalse(analisys2.equals(analisys1));

		analisys2.addPredicate(PREDICATE_A);
		assertTrue(analisys1.equals(analisys2));
		assertTrue(analisys2.equals(analisys1));
		assertTrue(analisys1.hashCode() == analisys2.hashCode());

		analisys1.addRule(rule1);
		assertFalse(analisys1.equals(analisys2));
		assertFalse(analisys2.equals(analisys1));

		analisys2.addRule(rule1);
		assertTrue(analisys1.equals(analisys2));
		assertTrue(analisys2.equals(analisys1));
		assertTrue(analisys1.hashCode() == analisys2.hashCode());
	}

	public void testGetEvidence() {
		Set evidence = analisys.getEvidence();
		assertEquals(1, evidence.size());
		assertTrue(evidence.contains(predicateC));
	}

	public void testGetHypotesys() {
		List hypotesis = analisys.getHypotesys();
		assertEquals(1, hypotesis.size());
		assertTrue(hypotesis.contains(predicateC));
	}

	public void testGetPostulate() {
		List postulate = analisys.getPostulate();
		assertEquals(2, postulate.size());
		assertTrue(postulate.contains(predicateA));
		assertTrue(postulate.contains(predicateB));
	}

	public void testGetPredicateString() {
		assertNull(analisys.getPredicate(NO_PREDICATE));
		assertEquals(predicateA, analisys.getPredicate(PREDICATE_A));
		assertEquals(predicateB, analisys.getPredicate(PREDICATE_B));
		assertEquals(predicateC, analisys.getPredicate(PREDICATE_C));
	}

}