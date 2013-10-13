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
public class AssignListCmdTest {
	@DataPoints
	public static String[] VALUES = { "A", "B" };
	private MockExecutionContext mock;

	@Before
	public void setUp() throws Exception {
		mock = new MockExecutionContext();
	}

	@Theory
	public void testExecute(String p1, String p2) {
		AssertListCmd cmd = new AssertListCmd(new AssertTrueCmd(p1),
				new AssertTrueCmd(p2));
		mock.push(FuzzyBoolean.UNKNOWN);
		cmd.execute(mock);
		assertThat(mock, hasProperty("size", equalTo(0)));
		assertThat(mock, hasProperty("predicate", equalTo(p2)));
		assertThat(mock, hasProperty("value", equalTo(FuzzyBoolean.UNKNOWN)));
	}

	@Theory
	public void testToString(String p1, String p2) {
		AssertListCmd cmd = new AssertListCmd(new AssertTrueCmd(p1),
				new AssertTrueCmd(p2));
		assertThat(cmd.toString(),
				equalTo("[" + p1 + "=true, " + p2 + "=true]"));
	}

}
