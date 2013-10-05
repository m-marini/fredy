package org.mmarini.fuzzy;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasProperty;
import static org.junit.Assert.assertThat;

import org.junit.Before;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

@RunWith(Theories.class)
public class SomewhatExpTest {
	@DataPoints
	public static FuzzyBoolean[] VALUES = { FuzzyBoolean.FALSE,
			FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.UNKNOWN,
			FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.TRUE };

	private MockExecutionContext mock;

	@Before
	public void setUp() throws Exception {
		mock = new MockExecutionContext();
	}

	@Theory
	public void testExecute(FuzzyBoolean value) {
		SomewhatExp exp = new SomewhatExp(new ConstantExp(value));
		exp.execute(mock);
		assertThat(mock, hasProperty("size", equalTo(1)));
		assertThat(mock, hasProperty("value", equalTo(value.somewhat())));
	}

	@Theory
	public void testToString(FuzzyBoolean value) {
		SomewhatExp exp = new SomewhatExp(new ConstantExp(value));
		assertThat(exp.toString(), equalTo("somewhat(" + value + ")"));
	}
}
