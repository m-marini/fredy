package org.mmarini.fuzzy;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.Matchers.hasProperty;
import static org.junit.Assert.assertThat;

import org.junit.Before;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

@RunWith(Theories.class)
public class ConstantExpTest {
	@DataPoints
	public static FuzzyBoolean[] VALUES = { FuzzyBoolean.FALSE,
			FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.UNKNOWN,
			FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.TRUE };
	private MockExecutionContext mock;

	@Before
	public void setUp() {
		mock = new MockExecutionContext();
	}

	@Theory
	public void testExecute(FuzzyBoolean value) {
		ConstantExp exp = new ConstantExp(value);
		exp.execute(mock);
		assertThat(mock, hasProperty("size", equalTo(1)));
		assertThat(mock, hasProperty("value", equalTo(value)));
	}

	@Theory
	public void testFalseToString(FuzzyBoolean value) {
		ConstantExp exp = new ConstantExp(value);
		assertThat(exp.toString(), equalTo(value.toString()));
	}

	@Theory
	public void testGetValue(FuzzyBoolean value) {
		ConstantExp exp = new ConstantExp(value);
		assertThat(exp, hasProperty("value", equalTo(value)));
	}
}
