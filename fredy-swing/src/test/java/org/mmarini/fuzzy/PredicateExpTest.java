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
public class PredicateExpTest {
	@DataPoints
	public static String[] VALUES = { "A", "B" };

	private MockExecutionContext mock;

	@Before
	public void setUp() throws Exception {
		mock = new MockExecutionContext();
	}

	@Theory
	public void testExecute(String value) {
		PredicateExp exp = new PredicateExp(value);
		exp.execute(mock);
		assertThat(mock, hasProperty("size", equalTo(1)));
		assertThat(mock, hasProperty("predicate", equalTo(value)));
		assertThat(mock, hasProperty("value", equalTo(FuzzyBoolean.UNKNOWN)));
	}

	@Theory
	public void testToString(String value) {
		PredicateExp exp = new PredicateExp(value);
		assertThat(exp.toString(), equalTo(value));
	}
}
