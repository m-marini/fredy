package org.mmarini.fuzzy;

import org.junit.Before;
import org.junit.Test;
import org.mmarini.fuzzy.Constant;
import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.IExpressionVisitor;
import org.mmarini.fuzzy.IPredicate;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.beans.HasPropertyWithValue.*;
import static org.junit.Assert.*;

/**
 * @author US00852
 * @version $Id: ConstantTest.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class ConstantTest {
	Constant equals1;
	Constant equals2;
	Constant falseConst;
	Constant quiteFalseConst;
	Constant trueConst;
	Constant quiteTrueConst;
	Constant unkonwnConst;
	FuzzyBoolean[] values;

	/*
	 * @see TestCase#setUp()
	 */
	@Before
	public void init() throws Exception {
		values = new FuzzyBoolean[] { FuzzyBoolean.FALSE,
				FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.UNKNOWN,
				FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.TRUE };
		equals1 = new Constant();
		equals2 = new Constant();
		falseConst = new Constant(FuzzyBoolean.FALSE);
		quiteFalseConst = new Constant(FuzzyBoolean.QUITE_FALSE);
		quiteTrueConst = new Constant(FuzzyBoolean.QUITE_TRUE);
		trueConst = new Constant(FuzzyBoolean.TRUE);
		unkonwnConst = new Constant(FuzzyBoolean.UNKNOWN);
	}

	/**
	 * 
	 */
	@Test
	public void testAcceptIExpressionVisitor() {
		equals1.accept(new IExpressionVisitor() {

			@Override
			public void visitPredicate(IPredicate predicate) {
				fail();
			}
		});
	}

	/**
	 * Class under test for void Constant()
	 */
	@Test
	public void testConstant() {
		assertThat(equals1, hasProperty("value", equalTo(FuzzyBoolean.UNKNOWN)));
	}

	/**
	 * Class under test for void Constant(FuzzyBoolean)
	 */
	@Test
	public void testConstantFuzzyBoolean() {
		assertThat(falseConst,
				hasProperty("value", equalTo(FuzzyBoolean.FALSE)));
		assertThat(trueConst, hasProperty("value", equalTo(FuzzyBoolean.TRUE)));
		assertThat(unkonwnConst,
				hasProperty("value", equalTo(FuzzyBoolean.UNKNOWN)));
		assertThat(quiteFalseConst,
				hasProperty("value", equalTo(FuzzyBoolean.QUITE_FALSE)));
		assertThat(quiteTrueConst,
				hasProperty("value", equalTo(FuzzyBoolean.QUITE_TRUE)));
	}

	/**
	 * Class under test for boolean equals(Object)
	 */
	@Test
	public void testEqualsObject() {
		assertThat(equals1.equals(equals1), is(equalTo(true)));
		assertThat(equals1.equals(null), is(equalTo(false)));
		assertThat(equals1.equals(new Object()), is(equalTo(false)));

		assertThat(equals1.equals(equals2), is(equalTo(true)));
		assertThat(equals2.equals(equals2), is(equalTo(true)));
		assertThat(equals2.hashCode() == equals2.hashCode(), is(equalTo(true)));

		for (int i = 0; i < values.length; ++i) {
			equals1.setValue(values[i]);
			assertThat(equals1.equals(equals2), is(equalTo(false)));
			assertThat(equals2.equals(equals1), is(equalTo(false)));

			equals2.setValue(values[i]);
			assertThat(equals1.equals(equals2), is(equalTo(true)));
			assertThat(equals2.equals(equals1), is(equalTo(true)));
			assertThat(equals1.hashCode() == equals2.hashCode(),
					is(equalTo(true)));
		}
	}

	/**
	 * 
	 */
	@Test
	public void testEvaluate() {
		assertThat(falseConst.evaluate(null), is(equalTo(FuzzyBoolean.FALSE)));
		assertThat(quiteFalseConst.evaluate(null),
				is(equalTo(FuzzyBoolean.QUITE_FALSE)));
		assertThat(unkonwnConst.evaluate(null),
				is(equalTo(FuzzyBoolean.UNKNOWN)));
		assertThat(quiteTrueConst.evaluate(null),
				is(equalTo(FuzzyBoolean.QUITE_TRUE)));
		assertThat(trueConst.evaluate(null), is(equalTo(FuzzyBoolean.TRUE)));
	}

	/**
	 * 
	 */
	@Test
	public void testSetValue() {
		for (int i = 0; i < values.length; ++i) {
			equals1.setValue(values[i]);
			assertThat(equals1, hasProperty("value", equalTo(values[i])));
		}
	}

	/**
	 * Class under test for String toString()
	 */
	@Test
	public void testToString() {
		assertThat(equals1.toString(),
				containsString(FuzzyBoolean.UNKNOWN.toString()));
	}
}