/*
 * Created on 10-nov-2004
 */
package org.mmarini.fuzzy;

import org.mmarini.fuzzy.FuzzyBoolean;

import junit.framework.TestCase;

/**
 * @author US00852
 * @version $Id: FuzzyBooleanTest.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class FuzzyBooleanTest extends TestCase {

	private static final String FALSE_STRING = FuzzyBoolean.FALSE_DESCRIPTION
			+ "(0.0)";
	private static final String QUITE_FALSE_STRING = FuzzyBoolean.QUITE_FALSE_DESCRIPTION
			+ "(0.25)";
	private static final String HALF_STRING = FuzzyBoolean.UNKNOWN_DESCRIPTION
			+ "(0.5)";
	private static final String QUITE_TRUE_STRING = FuzzyBoolean.QUITE_TRUE_DESCRIPTION
			+ "(0.75)";
	private static final String TRUE_STRING = FuzzyBoolean.TRUE_DESCRIPTION
			+ "(1.0)";

	private static final double DEFAULT_VALUE = 0.;
	private static final double EPSILON = 1e-10;

	FuzzyBoolean fuzzy;
	FuzzyBoolean fuzzy1;

	FuzzyBoolean[] values;
	FuzzyBoolean[] notExp;
	FuzzyBoolean[] veryExp;
	FuzzyBoolean[] somewhatExp;
	FuzzyBoolean[] knownExp;
	FuzzyBoolean[] andExp;
	FuzzyBoolean[] orExp;
	FuzzyBoolean[] ifonlyifExp;
	FuzzyBoolean[] impliesExp;

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		fuzzy = new FuzzyBoolean();
		fuzzy1 = new FuzzyBoolean();
		values = new FuzzyBoolean[] { FuzzyBoolean.FALSE,
				FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.UNKNOWN,
				FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.TRUE };
		notExp = new FuzzyBoolean[] { FuzzyBoolean.TRUE,
				FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.UNKNOWN,
				FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.FALSE };
		knownExp = new FuzzyBoolean[] { FuzzyBoolean.TRUE,
				FuzzyBoolean.UNKNOWN, FuzzyBoolean.FALSE, FuzzyBoolean.UNKNOWN,
				FuzzyBoolean.TRUE };
		veryExp = new FuzzyBoolean[] {
				FuzzyBoolean.FALSE,
				new FuzzyBoolean(FuzzyBoolean.QUITE_FALSE_VALUE
						* FuzzyBoolean.QUITE_FALSE_VALUE),
				new FuzzyBoolean(FuzzyBoolean.QUITE_FALSE),
				new FuzzyBoolean(FuzzyBoolean.QUITE_TRUE_VALUE
						* FuzzyBoolean.QUITE_TRUE_VALUE), FuzzyBoolean.TRUE };
		somewhatExp = new FuzzyBoolean[] { FuzzyBoolean.FALSE,
				FuzzyBoolean.UNKNOWN,
				new FuzzyBoolean(Math.sqrt(FuzzyBoolean.UNKNOWN_VALUE)),
				new FuzzyBoolean(Math.sqrt(FuzzyBoolean.QUITE_TRUE_VALUE)),
				FuzzyBoolean.TRUE };
		andExp = new FuzzyBoolean[] { FuzzyBoolean.FALSE, FuzzyBoolean.FALSE,
				FuzzyBoolean.FALSE, FuzzyBoolean.FALSE, FuzzyBoolean.FALSE,

				FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_FALSE,
				FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_FALSE,

				FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN,
				FuzzyBoolean.UNKNOWN,

				FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_TRUE,

				FuzzyBoolean.TRUE };
		orExp = new FuzzyBoolean[] { FuzzyBoolean.FALSE,
				FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.UNKNOWN,
				FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.TRUE,

				FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.UNKNOWN,
				FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.TRUE,

				FuzzyBoolean.UNKNOWN, FuzzyBoolean.QUITE_TRUE,
				FuzzyBoolean.TRUE,

				FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.TRUE,

				FuzzyBoolean.TRUE };
		ifonlyifExp = new FuzzyBoolean[] { FuzzyBoolean.TRUE,
				FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.UNKNOWN,
				FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.FALSE,

				FuzzyBoolean.TRUE, FuzzyBoolean.QUITE_TRUE,
				FuzzyBoolean.UNKNOWN, FuzzyBoolean.QUITE_FALSE,

				FuzzyBoolean.TRUE, FuzzyBoolean.QUITE_TRUE,
				FuzzyBoolean.UNKNOWN,

				FuzzyBoolean.TRUE, FuzzyBoolean.QUITE_TRUE,

				FuzzyBoolean.TRUE };
		impliesExp = new FuzzyBoolean[] { FuzzyBoolean.TRUE, FuzzyBoolean.TRUE,
				FuzzyBoolean.TRUE, FuzzyBoolean.TRUE, FuzzyBoolean.TRUE,

				FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.TRUE, FuzzyBoolean.TRUE,
				FuzzyBoolean.TRUE, FuzzyBoolean.TRUE,

				FuzzyBoolean.UNKNOWN, FuzzyBoolean.QUITE_TRUE,
				FuzzyBoolean.TRUE, FuzzyBoolean.TRUE, FuzzyBoolean.TRUE,

				FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.UNKNOWN,
				FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.TRUE, FuzzyBoolean.TRUE,

				FuzzyBoolean.FALSE, FuzzyBoolean.QUITE_FALSE,
				FuzzyBoolean.UNKNOWN, FuzzyBoolean.QUITE_TRUE,
				FuzzyBoolean.TRUE };
	}

	public void testAnd() {
		int idx = 0;
		for (int i = 0; i < values.length; ++i) {
			for (int j = i; j < values.length; ++j) {
				FuzzyBoolean value1 = values[i];
				FuzzyBoolean value2 = values[j];
				FuzzyBoolean value = value1.and(value2);
				assertEquals(values[i] + "," + values[j], andExp[idx], value);
				value = value2.and(value1);
				assertEquals(values[j] + "," + values[i], andExp[idx], value);
				++idx;
			}
		}
	}

	public void testCompareTo() {
		for (int i = 0; i < values.length; ++i) {
			if (i > 0)
				assertTrue(values[i] + "," + values[i - 1],
						values[i].compareTo(values[i - 1]) > 0);
			assertTrue(values[i] + "," + values[i],
					values[i].compareTo(values[i]) == 0);
			if (i < values.length - 1)
				assertTrue(values[i] + "," + values[i + 1],
						values[i].compareTo(values[i + 1]) < 0);
		}
	}

	/*
	 * Class under test for boolean equals(Object)
	 */
	public void testEqualsObject() {
		assertTrue(fuzzy.equals(fuzzy));
		assertFalse(fuzzy.equals(null));
		assertFalse(fuzzy.equals(new Object()));

		assertTrue(fuzzy.equals(fuzzy1));
		assertTrue(fuzzy1.equals(fuzzy));
		assertTrue(fuzzy.hashCode() == fuzzy1.hashCode());

		fuzzy = new FuzzyBoolean(FuzzyBoolean.UNKNOWN);
		assertFalse(fuzzy.equals(fuzzy1));
		assertFalse(fuzzy1.equals(fuzzy));

		fuzzy1 = new FuzzyBoolean(FuzzyBoolean.UNKNOWN);
		assertTrue(fuzzy.equals(fuzzy1));
		assertTrue(fuzzy1.equals(fuzzy));
		assertTrue(fuzzy.hashCode() == fuzzy1.hashCode());

		fuzzy = new FuzzyBoolean(FuzzyBoolean.TRUE);
		assertFalse(fuzzy.equals(fuzzy1));
		assertFalse(fuzzy1.equals(fuzzy));

		fuzzy1 = new FuzzyBoolean(FuzzyBoolean.TRUE);
		assertTrue(fuzzy.equals(fuzzy1));
		assertTrue(fuzzy1.equals(fuzzy));
		assertTrue(fuzzy.hashCode() == fuzzy1.hashCode());
	}

	/*
	 * Class under test for void FuzzyValue()
	 */
	public void testFuzzyValue() {
		assertEquals(DEFAULT_VALUE, fuzzy.getValue(), EPSILON);
	}

	/*
	 * Class under test for void FuzzyValue(double)
	 */
	public void testFuzzyValuedouble() {
		for (int i = 0; i < values.length; ++i) {
			FuzzyBoolean value = new FuzzyBoolean(values[i].getValue());
			assertEquals(values[i], value);
			assertNotSame(values[i], value);
		}
	}

	/*
	 * Class under test for void FuzzyValue(FuzzyValue)
	 */
	public void testFuzzyValueFuzzyValue() {
		for (int i = 0; i < values.length; ++i) {
			FuzzyBoolean value = new FuzzyBoolean(values[i]);
			assertEquals(values[i], value);
			assertNotSame(values[i], value);
		}
	}

	/*
	 * Class under test for String toString()
	 */
	public void testGetDescription() {
		assertEquals(FuzzyBoolean.FALSE_DESCRIPTION,
				FuzzyBoolean.FALSE.getDescription());
		assertEquals(FuzzyBoolean.QUITE_FALSE_DESCRIPTION,
				FuzzyBoolean.QUITE_FALSE.getDescription());
		assertEquals(FuzzyBoolean.UNKNOWN_DESCRIPTION,
				FuzzyBoolean.UNKNOWN.getDescription());
		assertEquals(FuzzyBoolean.QUITE_TRUE_DESCRIPTION,
				FuzzyBoolean.QUITE_TRUE.getDescription());
		assertEquals(FuzzyBoolean.TRUE_DESCRIPTION,
				FuzzyBoolean.TRUE.getDescription());
	}

	public void testIfonlyif() {
		int idx = 0;
		for (int i = 0; i < values.length; ++i) {
			for (int j = i; j < values.length; ++j) {
				FuzzyBoolean value1 = values[i];
				FuzzyBoolean value2 = values[j];
				FuzzyBoolean value = value1.ifonlyif(value2);
				assertEquals(values[i] + "," + values[j], ifonlyifExp[idx],
						value);
				value = value2.ifonlyif(value1);
				assertEquals(values[j] + "," + values[i], ifonlyifExp[idx],
						value);
				++idx;
			}
		}
	}

	public void testImplies() {
		int idx = 0;
		for (int i = 0; i < values.length; ++i) {
			for (int j = 0; j < values.length; ++j) {
				FuzzyBoolean value1 = values[i];
				FuzzyBoolean value2 = values[j];
				FuzzyBoolean value = value1.implies(value2);
				assertEquals(values[i] + "," + values[j], impliesExp[idx],
						value);
				++idx;
			}
		}
	}

	/*
	 * Class under test for String toString()
	 */
	public void testIsFalse() {
		assertTrue(FuzzyBoolean.FALSE.isFalse());
		assertFalse(FuzzyBoolean.QUITE_FALSE.isFalse());
		assertFalse(FuzzyBoolean.UNKNOWN.isFalse());
		assertFalse(FuzzyBoolean.QUITE_TRUE.isFalse());
		assertFalse(FuzzyBoolean.TRUE.isFalse());
	}

	/*
	 * Class under test for String toString()
	 */
	public void testIsHalfTrue() {
		assertFalse(FuzzyBoolean.FALSE.isUnknown());
		assertFalse(FuzzyBoolean.QUITE_FALSE.isUnknown());
		assertTrue(FuzzyBoolean.UNKNOWN.isUnknown());
		assertFalse(FuzzyBoolean.QUITE_TRUE.isUnknown());
		assertFalse(FuzzyBoolean.TRUE.isUnknown());
	}

	public void testIsKnown() {
		assertTrue(FuzzyBoolean.FALSE.isKnown());
		assertFalse(FuzzyBoolean.QUITE_FALSE.isKnown());
		assertFalse(FuzzyBoolean.UNKNOWN.isKnown());
		assertFalse(FuzzyBoolean.QUITE_TRUE.isKnown());
		assertTrue(FuzzyBoolean.TRUE.isKnown());
	}

	/*
	 * Class under test for String toString()
	 */
	public void testIsQuiteFalse() {
		assertFalse(FuzzyBoolean.FALSE.isQuiteFalse());
		assertTrue(FuzzyBoolean.QUITE_FALSE.isQuiteFalse());
		assertFalse(FuzzyBoolean.UNKNOWN.isQuiteFalse());
		assertFalse(FuzzyBoolean.QUITE_TRUE.isQuiteFalse());
		assertFalse(FuzzyBoolean.TRUE.isQuiteFalse());
	}

	/*
	 * Class under test for String toString()
	 */
	public void testIsQuiteTrue() {
		assertFalse(FuzzyBoolean.FALSE.isQuiteTrue());
		assertFalse(FuzzyBoolean.QUITE_FALSE.isQuiteTrue());
		assertFalse(FuzzyBoolean.UNKNOWN.isQuiteTrue());
		assertTrue(FuzzyBoolean.QUITE_TRUE.isQuiteTrue());
		assertFalse(FuzzyBoolean.TRUE.isQuiteTrue());
	}

	/*
	 * Class under test for String toString()
	 */
	public void testIsTrue() {
		assertFalse(FuzzyBoolean.FALSE.isTrue());
		assertFalse(FuzzyBoolean.QUITE_FALSE.isTrue());
		assertFalse(FuzzyBoolean.UNKNOWN.isTrue());
		assertFalse(FuzzyBoolean.QUITE_TRUE.isTrue());
		assertTrue(FuzzyBoolean.TRUE.isTrue());
	}

	public void testKnwon() {
		for (int i = 0; i < values.length; ++i) {
			assertEquals(values[i].toString(), knownExp[i], values[i].known());
		}
	}

	public void testNot() {
		for (int i = 0; i < values.length; ++i) {
			assertEquals(values[i].toString(), notExp[i], values[i].not());
		}
	}

	public void testOr() {
		int idx = 0;
		for (int i = 0; i < values.length; ++i) {
			for (int j = i; j < values.length; ++j) {
				FuzzyBoolean value1 = values[i];
				FuzzyBoolean value2 = values[j];
				FuzzyBoolean value = value1.or(value2);
				assertEquals(values[i] + "," + values[j], orExp[idx], value);
				value = value2.or(value1);
				assertEquals(values[j] + "," + values[i], orExp[idx], value);
				++idx;
			}
		}
	}

	public void testSomewhat() {
		for (int i = 0; i < values.length; ++i) {
			assertEquals(values[i].toString(), somewhatExp[i],
					values[i].somewhat());
		}
	}

	/*
	 * Class under test for String toString()
	 */
	public void testToString() {
		assertEquals(FALSE_STRING, FuzzyBoolean.FALSE.toString());
		assertEquals(QUITE_FALSE_STRING, FuzzyBoolean.QUITE_FALSE.toString());
		assertEquals(HALF_STRING, FuzzyBoolean.UNKNOWN.toString());
		assertEquals(QUITE_TRUE_STRING, FuzzyBoolean.QUITE_TRUE.toString());
		assertEquals(TRUE_STRING, FuzzyBoolean.TRUE.toString());
	}

	public void testVery() {
		for (int i = 0; i < values.length; ++i) {
			assertEquals(values[i].toString(), veryExp[i], values[i].very());
		}
	}
}