package org.mmarini.fuzzy;

import org.mmarini.fuzzy.Constant;
import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.IExpressionVisitor;
import org.mmarini.fuzzy.IPredicate;

import junit.framework.TestCase;

/**
 * @author US00852
 * @version $Id: ConstantTest.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class ConstantTest extends TestCase {
	Constant equals1;
	Constant equals2;
	FuzzyBoolean[] values = new FuzzyBoolean[] { FuzzyBoolean.FALSE,
			FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.UNKNOWN,
			FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.TRUE };

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		equals1 = new Constant();
		equals2 = new Constant();
	}

	public void testAcceptIExpressionVisitor() {
		equals1.accept(new IExpressionVisitor() {

			@Override
			public void visitPredicate(IPredicate predicate) {
				fail();
			}
		});
	}

	/*
	 * Class under test for void Constant()
	 */
	public void testConstant() {
		assertEquals(FuzzyBoolean.UNKNOWN, equals1.getValue());
	}

	/*
	 * Class under test for void Constant(FuzzyBoolean)
	 */
	public void testConstantFuzzyBoolean() {
		for (int i = 0; i < values.length; ++i) {
			Constant constant = new Constant(values[i]);
			assertEquals(values[i], constant.getValue());
		}
	}

	/*
	 * Class under test for boolean equals(Object)
	 */
	public void testEqualsObject() {
		assertTrue(equals1.equals(equals1));
		assertFalse(equals1.equals(null));
		assertFalse(equals1.equals(new Object()));

		assertTrue(equals1.equals(equals2));
		assertTrue(equals2.equals(equals1));
		assertTrue(equals1.hashCode() == equals2.hashCode());

		for (int i = 0; i < values.length; ++i) {
			equals1.setValue(values[i]);
			assertFalse(equals1.equals(equals2));
			assertFalse(equals2.equals(equals1));

			equals2.setValue(values[i]);
			assertTrue(equals1.equals(equals2));
			assertTrue(equals2.equals(equals1));
			assertTrue(equals1.hashCode() == equals2.hashCode());
		}
	}

	public void testEvaluate() {
		for (int i = 0; i < values.length; ++i) {
			equals1.setValue(values[i]);
			assertEquals(values[i], equals1.evaluate(null));
		}
	}

	public void testSetValue() {
		for (int i = 0; i < values.length; ++i) {
			equals1.setValue(values[i]);
			assertEquals(values[i], equals1.getValue());
		}
	}

	/*
	 * Class under test for String toString()
	 */
	public void testToString() {
		assertEquals(FuzzyBoolean.UNKNOWN.toString(), equals1.toString());
	}
}