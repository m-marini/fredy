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
public class IfOnlyIfExpTest {
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
	public void testExecute(FuzzyBoolean value1, FuzzyBoolean value2) {
		IfOnlyIfExp exp = new IfOnlyIfExp(new ConstantExp(value1),
				new ConstantExp(value2));
		exp.execute(mock);
		assertThat(mock, hasProperty("size", equalTo(1)));
		assertThat(mock, hasProperty("value", equalTo(value1.ifonlyif(value2))));
	}

	@Theory
	public void testToString(FuzzyBoolean value1, FuzzyBoolean value2) {
		IfOnlyIfExp exp = new IfOnlyIfExp(new ConstantExp(value1),
				new ConstantExp(value2));
		assertThat(exp.toString(), equalTo("ifOnlyIf[" + value1 + ", " + value2
				+ "]"));
	}
}
