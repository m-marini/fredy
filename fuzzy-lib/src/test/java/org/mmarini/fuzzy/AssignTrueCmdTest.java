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
public class AssignTrueCmdTest {
	@DataPoints
	public static String[] PERDICATE = { "A", "B" };
	@DataPoints
	public static FuzzyBoolean[] VALUES = { FuzzyBoolean.FALSE,
			FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.UNKNOWN,
			FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.TRUE };

	private MockExecutionContext mock;

	@Theory
	public void assignExecute(String predicate, FuzzyBoolean value) {
		AssertTrueCmd cmd = new AssertTrueCmd(predicate);
		mock.push(value);
		cmd.execute(mock);
		assertThat(mock, hasProperty("size", equalTo(0)));
		assertThat(mock, hasProperty("predicate", equalTo(predicate)));
		assertThat(mock, hasProperty("value", equalTo(value)));
	}

	@Theory
	public void assignHasPredicate(String predicate) {
		AssertTrueCmd cmd = new AssertTrueCmd(predicate);
		assertThat(cmd, hasProperty("predicate", equalTo(predicate)));
	}

	@Theory
	public void assignToString(String predicate) {
		AssertTrueCmd cmd = new AssertTrueCmd(predicate);
		assertThat(cmd.toString(), equalTo(predicate + "=true"));
	}

	@Before
	public void setUp() throws Exception {
		mock = new MockExecutionContext();
	}
}
