/*
 * Created on 10-nov-2004
 */
package org.mmarini.fuzzy;

import static org.hamcrest.Matchers.closeTo;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasProperty;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;

/**
 * @author US00852
 * @version $Id: FuzzyBooleanTest.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class FuzzyBooleanTest {

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
	@Before
	public void init() {
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

	@Test
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

	@Test
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
	@Test
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
	@Test
	public void testFuzzyValue() {
		assertThat(fuzzy, hasProperty("value", closeTo(DEFAULT_VALUE, EPSILON)));
	}

	/*
	 * Class under test for void FuzzyValue(double)
	 */
	@Test
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
	@Test
	public void testFuzzyValueFuzzyValue() {
		for (int i = 0; i < values.length; ++i) {
			FuzzyBoolean value = new FuzzyBoolean(values[i]);
			assertEquals(values[i], value);
			assertNotSame(values[i], value);
		}
	}

	@Test
	public void testGetDescriptionFalse() {
		assertThat(
				FuzzyBoolean.FALSE,
				hasProperty("description",
						equalTo(FuzzyBoolean.FALSE_DESCRIPTION)));
	}

	@Test
	public void testGetDescriptionQuiteFalse() {
		assertThat(
				FuzzyBoolean.QUITE_FALSE,
				hasProperty("description",
						equalTo(FuzzyBoolean.QUITE_FALSE_DESCRIPTION)));
	}

	@Test
	public void testGetDescriptionQuiteTrue() {
		assertThat(
				FuzzyBoolean.QUITE_TRUE,
				hasProperty("description",
						equalTo(FuzzyBoolean.QUITE_TRUE_DESCRIPTION)));
	}

	@Test
	public void testGetDescriptionTrue() {
		assertThat(
				FuzzyBoolean.TRUE,
				hasProperty("description",
						equalTo(FuzzyBoolean.TRUE_DESCRIPTION)));
	}

	@Test
	public void testGetDescriptionUnknown() {
		assertThat(
				FuzzyBoolean.UNKNOWN,
				hasProperty("description",
						equalTo(FuzzyBoolean.UNKNOWN_DESCRIPTION)));
	}

	@Test
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

	@Test
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

	@Test
	public void testIsFalse1() {
		assertThat(FuzzyBoolean.FALSE, hasProperty("false", equalTo(true)));
	}

	@Test
	public void testIsFalse2() {
		assertThat(FuzzyBoolean.QUITE_FALSE,
				hasProperty("false", equalTo(false)));
	}

	@Test
	public void testIsFalse3() {
		assertThat(FuzzyBoolean.UNKNOWN, hasProperty("false", equalTo(false)));
	}

	@Test
	public void testIsFalse4() {
		assertThat(FuzzyBoolean.QUITE_TRUE,
				hasProperty("false", equalTo(false)));
	}

	@Test
	public void testIsFalse5() {
		assertThat(FuzzyBoolean.TRUE, hasProperty("false", equalTo(false)));
	}

	@Test
	public void testIsHalfTrue1() {
		assertThat(FuzzyBoolean.FALSE, hasProperty("unknown", equalTo(false)));
	}

	@Test
	public void testIsHalfTrue2() {
		assertThat(FuzzyBoolean.QUITE_FALSE,
				hasProperty("unknown", equalTo(false)));
	}

	@Test
	public void testIsHalfTrue3() {
		assertThat(FuzzyBoolean.UNKNOWN, hasProperty("unknown", equalTo(true)));
	}

	@Test
	public void testIsHalfTrue4() {
		assertThat(FuzzyBoolean.QUITE_TRUE,
				hasProperty("unknown", equalTo(false)));
	}

	@Test
	public void testIsHalfTrue5() {
		assertThat(FuzzyBoolean.TRUE, hasProperty("unknown", equalTo(false)));
	}

	@Test
	public void testIsKnown1() {
		assertThat(FuzzyBoolean.FALSE, hasProperty("known", equalTo(true)));
	}

	@Test
	public void testIsKnown2() {
		assertThat(FuzzyBoolean.QUITE_FALSE,
				hasProperty("known", equalTo(false)));
	}

	@Test
	public void testIsKnown3() {
		assertThat(FuzzyBoolean.UNKNOWN, hasProperty("known", equalTo(false)));
	}

	@Test
	public void testIsKnown4() {
		assertThat(FuzzyBoolean.QUITE_TRUE,
				hasProperty("known", equalTo(false)));
	}

	@Test
	public void testIsKnown5() {
		assertThat(FuzzyBoolean.TRUE, hasProperty("known", equalTo(true)));
	}

	@Test
	public void testIsQuiteFalse1() {
		assertThat(FuzzyBoolean.FALSE,
				hasProperty("quiteFalse", equalTo(false)));
	}

	@Test
	public void testIsQuiteFalse2() {
		assertThat(FuzzyBoolean.QUITE_FALSE,
				hasProperty("quiteFalse", equalTo(true)));
	}

	@Test
	public void testIsQuiteFalse3() {
		assertThat(FuzzyBoolean.UNKNOWN,
				hasProperty("quiteFalse", equalTo(false)));
	}

	@Test
	public void testIsQuiteFalse4() {
		assertThat(FuzzyBoolean.QUITE_TRUE,
				hasProperty("quiteFalse", equalTo(false)));
	}

	@Test
	public void testIsQuiteFalse5() {
		assertThat(FuzzyBoolean.TRUE, hasProperty("quiteFalse", equalTo(false)));
	}

	@Test
	public void testIsQuiteTrue1() {
		assertThat(FuzzyBoolean.FALSE, hasProperty("quiteTrue", equalTo(false)));
	}

	@Test
	public void testIsQuiteTrue2() {
		assertThat(FuzzyBoolean.QUITE_FALSE,
				hasProperty("quiteTrue", equalTo(false)));
	}

	@Test
	public void testIsQuiteTrue3() {
		assertThat(FuzzyBoolean.UNKNOWN,
				hasProperty("quiteTrue", equalTo(false)));
	}

	@Test
	public void testIsQuiteTrue4() {
		assertThat(FuzzyBoolean.QUITE_TRUE,
				hasProperty("quiteTrue", equalTo(true)));
	}

	@Test
	public void testIsQuiteTrue5() {
		assertThat(FuzzyBoolean.TRUE, hasProperty("quiteTrue", equalTo(false)));
	}

	@Test
	public void testIsTrue1() {
		assertThat(FuzzyBoolean.FALSE, hasProperty("true", equalTo(false)));
	}

	@Test
	public void testIsTrue2() {
		assertThat(FuzzyBoolean.QUITE_FALSE,
				hasProperty("true", equalTo(false)));
	}

	@Test
	public void testIsTrue3() {
		assertThat(FuzzyBoolean.UNKNOWN, hasProperty("true", equalTo(false)));
	}

	@Test
	public void testIsTrue4() {
		assertThat(FuzzyBoolean.QUITE_TRUE, hasProperty("true", equalTo(false)));
	}

	@Test
	public void testIsTrue5() {
		assertThat(FuzzyBoolean.TRUE, hasProperty("true", equalTo(true)));
	}

	@Test
	public void testKnwon() {
		for (int i = 0; i < values.length; ++i) {
			assertEquals(values[i].toString(), knownExp[i], values[i].known());
		}
	}

	@Test
	public void testNot() {
		for (int i = 0; i < values.length; ++i) {
			assertEquals(values[i].toString(), notExp[i], values[i].not());
		}
	}

	@Test
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

	@Test
	public void testSomewhat() {
		for (int i = 0; i < values.length; ++i) {
			assertEquals(values[i].toString(), somewhatExp[i],
					values[i].somewhat());
		}
	}

	@Test
	public void testToStringFalse() {
		assertThat(FuzzyBoolean.FALSE.toString(), containsString(FALSE_STRING));
	}

	@Test
	public void testToStringQuiteFalse() {
		assertThat(FuzzyBoolean.QUITE_FALSE.toString(),
				containsString(QUITE_FALSE_STRING));
	}

	@Test
	public void testToStringQuiteTrue() {
		assertThat(FuzzyBoolean.QUITE_TRUE.toString(),
				containsString(QUITE_TRUE_STRING));
	}

	@Test
	public void testToStringTrue() {
		assertThat(FuzzyBoolean.TRUE.toString(), containsString(TRUE_STRING));
	}

	@Test
	public void testToStringUnknown() {
		assertThat(FuzzyBoolean.UNKNOWN.toString(), containsString(HALF_STRING));
	}

	@Test
	public void testVery() {
		for (int i = 0; i < values.length; ++i) {
			assertEquals(values[i].toString(), veryExp[i], values[i].very());
		}
	}
}