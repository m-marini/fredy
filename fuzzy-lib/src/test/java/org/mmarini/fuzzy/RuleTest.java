package org.mmarini.fuzzy;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.Matchers.theInstance;
import static org.junit.Assert.assertThat;
import static org.junit.Assume.assumeTrue;

import org.junit.Before;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

@RunWith(Theories.class)
public class RuleTest {
	@DataPoints
	public static FuzzyBoolean[] EXPS = { FuzzyBoolean.FALSE,
			FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.UNKNOWN,
			FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.TRUE };
	@DataPoints
	public static String[] CONS = { "A", "B" };
	private MockExecutionContext mock;

	@Before
	public void setUp() throws Exception {
		mock = new MockExecutionContext();
	}

	@Theory
	public void testExecute(FuzzyBoolean value, String thenAss, String elseAss) {
		assumeTrue(!thenAss.equals(elseAss));
		ConstantExp cond = new ConstantExp(value);
		AssertListCmd thenCons = new AssertListCmd(new AssertTrueCmd(thenAss));
		AssertListCmd elseCons = new AssertListCmd(new AssertTrueCmd(elseAss));
		Rule r = new Rule(cond, thenCons, elseCons);
		r.execute(mock);
		assertThat(mock, hasProperty("size", equalTo(0)));
		assertThat(mock, hasProperty("value", equalTo(value.not())));
		assertThat(mock, hasProperty("predicate", equalTo(elseAss)));
	}

	@Theory
	public void testGetCondition(FuzzyBoolean value, String thenAss,
			String elseAss) {
		ConstantExp cond = new ConstantExp(value);
		AssertListCmd thenCons = new AssertListCmd(new AssertTrueCmd(thenAss));
		AssertListCmd elseCons = new AssertListCmd(new AssertTrueCmd(elseAss));
		Rule r = new Rule(cond, thenCons, elseCons);
		assertThat(r, hasProperty("condition", theInstance(cond)));
	}

	@Theory
	public void testGetElseConseguences(FuzzyBoolean value, String thenAss,
			String elseAss) {
		ConstantExp cond = new ConstantExp(value);
		AssertListCmd thenCons = new AssertListCmd(new AssertTrueCmd(thenAss));
		AssertListCmd elseCons = new AssertListCmd(new AssertTrueCmd(elseAss));
		Rule r = new Rule(cond, thenCons, elseCons);
		assertThat(r, hasProperty("elseConseguences", theInstance(elseCons)));
	}

	@Theory
	public void testGetThenConseguences(FuzzyBoolean value, String thenAss,
			String elseAss) {
		ConstantExp cond = new ConstantExp(value);
		AssertListCmd thenCons = new AssertListCmd(new AssertTrueCmd(thenAss));
		AssertListCmd elseCons = new AssertListCmd(new AssertTrueCmd(elseAss));
		Rule r = new Rule(cond, thenCons, elseCons);
		assertThat(r, hasProperty("thenConseguences", theInstance(thenCons)));
	}

	@Theory
	public void testToString(FuzzyBoolean value, String thenAss, String elseAss) {
		ConstantExp cond = new ConstantExp(value);
		AssertListCmd thenCons = new AssertListCmd(new AssertTrueCmd(thenAss));
		AssertListCmd elseCons = new AssertListCmd(new AssertFalseCmd(elseAss));
		Rule r = new Rule(cond, thenCons, elseCons);
		assertThat(r.toString(), equalTo("if " + value + " then [" + thenAss
				+ "=true] else [" + elseAss + "=false]"));
	}

}
