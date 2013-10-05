package org.mmarini.fuzzy;

import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.Predicate;
import org.mmarini.fuzzy.Rule;
import org.mmarini.fuzzy.Weight;

import junit.framework.TestCase;

/**
 * @author US00852
 * @version $Id: PredicateTest.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class PredicateTest extends TestCase {
	private static final Weight WEIGHT = new Weight(1, 2);
	public static final String NAME = "AName";
	private static final String PREDICATE_STRING = "Predicate(null,unknown(0.5))";
	private static final String PREDICATE_NAME_STRING = "Predicate(" + NAME
			+ ",unknown(0.5))";

	Predicate predicate1;
	Predicate predicate2;
	Predicate predicateName1;
	Predicate predicateName2;
	Predicate hypotesys;
	Rule rule;

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		predicate1 = new Predicate();
		predicate2 = new Predicate();
		predicateName1 = new Predicate(NAME);
		predicateName2 = new Predicate(NAME);
		hypotesys = new Predicate();
		rule = new Rule();
	}

	public void testEqualsObject() {
		assertTrue(predicate1.equals(predicate1));
		assertFalse(predicate1.equals(null));
		assertFalse(predicate1.equals(new Object()));

		assertTrue(predicate1.equals(predicate2));
		assertTrue(predicate2.equals(predicate1));
		assertTrue(predicate1.hashCode() == predicate2.hashCode());

		predicate1.setValue(FuzzyBoolean.TRUE);
		assertTrue(predicate1.equals(predicate2));
		assertTrue(predicate2.equals(predicate1));
		assertTrue(predicate1.hashCode() == predicate2.hashCode());

		predicate2.setValue(FuzzyBoolean.TRUE);
		assertTrue(predicate1.equals(predicate2));
		assertTrue(predicate2.equals(predicate1));
		assertTrue(predicate1.hashCode() == predicate2.hashCode());

		predicate1.setWeight(WEIGHT);
		assertTrue(predicate1.equals(predicate2));
		assertTrue(predicate2.equals(predicate1));
		assertTrue(predicate1.hashCode() == predicate2.hashCode());

		predicate1.setWeight(WEIGHT);
		assertTrue(predicate1.equals(predicate2));
		assertTrue(predicate2.equals(predicate1));
		assertTrue(predicate1.hashCode() == predicate2.hashCode());

		assertFalse(predicateName1.equals(predicate2));
		assertFalse(predicate2.equals(predicateName1));

		assertTrue(predicateName1.equals(predicateName2));
		assertTrue(predicateName2.equals(predicateName1));
		assertTrue(predicateName1.hashCode() == predicateName2.hashCode());

		predicate1.setWeight(WEIGHT);
	}

	/*
	 * Class under test for void Predicate(String)
	 */
	public void testPredicate() {
		assertEquals(FuzzyBoolean.UNKNOWN, predicate1.getValue());
		assertEquals(Weight.NULL_WEIGHT, predicate1.getWeight());
	}

	/*
	 * Class under test for void Predicate(String)
	 */
	public void testPredicateString() {
		assertEquals(NAME, predicateName1.getName());
		assertEquals(FuzzyBoolean.UNKNOWN, predicate1.getValue());
		assertEquals(Weight.NULL_WEIGHT, predicate1.getWeight());
	}

	public void testSetValue() {
		predicate1.setValue(FuzzyBoolean.TRUE);
		assertSame(FuzzyBoolean.TRUE, predicate1.getValue());
	}

	public void testSetWeight() {
		predicate1.setWeight(WEIGHT);
		assertSame(WEIGHT, predicate1.getWeight());
	}

	/*
	 * Class under test for String toString()
	 */
	public void testToString() {
		assertEquals(PREDICATE_STRING, predicate1.toString());
		assertEquals(PREDICATE_NAME_STRING, predicateName1.toString());
	}
}