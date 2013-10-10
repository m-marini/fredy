package org.mmarini.fuzzy;

import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.junit.Before;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.ParameterSignature;
import org.junit.experimental.theories.ParameterSupplier;
import org.junit.experimental.theories.ParametersSuppliedBy;
import org.junit.experimental.theories.PotentialAssignment;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;
import org.mmarini.functional.FSet;

@RunWith(Theories.class)
public class InferenceEngineTest {
	@Retention(RetentionPolicy.RUNTIME)
	@ParametersSuppliedBy(RuleListSupplier.class)
	public @interface RuleList {
	}

	public static class RuleListSupplier extends ParameterSupplier {

		/**
		 * Datapoint:
		 * <p>
		 * 
		 * <pre>
		 * A -> D
		 * B -> D -> F
		 * C -> E -> F
		 * C -> G
		 * </pre>
		 * 
		 * </p>
		 */
		@Override
		public List<PotentialAssignment> getValueSources(ParameterSignature arg0) {
			ArrayList<Rule> rules = new ArrayList<Rule>();
			rules.add(new Rule(new PredicateExp("A"), new AssignListCmd(
					new AssignTrueCmd("D")), new AssignListCmd()));
			rules.add(new Rule(new PredicateExp("B"), new AssignListCmd(
					new AssignTrueCmd("D")), new AssignListCmd()));
			rules.add(new Rule(new PredicateExp("D"), new AssignListCmd(
					new AssignTrueCmd("F")), new AssignListCmd()));
			rules.add(new Rule(new PredicateExp("C"), new AssignListCmd(
					new AssignTrueCmd("E"), new AssignTrueCmd("G")),
					new AssignListCmd()));
			rules.add(new Rule(new PredicateExp("E"), new AssignListCmd(
					new AssignTrueCmd("F")), new AssignListCmd()));
			ArrayList<PotentialAssignment> arrayList = new ArrayList<PotentialAssignment>();
			arrayList.add(PotentialAssignment.forValue("rules", rules));
			return arrayList;
		}
	}

	@DataPoints
	public static FuzzyBoolean[] VALUES = { FuzzyBoolean.FALSE,
			FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.UNKNOWN,
			FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.TRUE };

	/**
	 * 
	 * @throws Exception
	 */
	@Before
	public void setUp() throws Exception {
	}

	@Theory
	public void testComputeClosure(@RuleList List<Rule> rules) {
		InferenceEngine e = new InferenceEngine();
		e.applyRules(rules);
		FSet<Relation> rs = e.addRelations();
		assertThat(rs, hasSize(6));
		assertThat(rs, hasItem(new Relation("A", "D")));
		assertThat(rs, hasItem(new Relation("B", "D")));
		assertThat(rs, hasItem(new Relation("B", "F")));
		assertThat(rs, hasItem(new Relation("C", "E")));
		assertThat(rs, hasItem(new Relation("C", "F")));
		assertThat(rs, hasItem(new Relation("C", "G")));
		assertThat(rs, hasItem(new Relation("D", "F")));
		assertThat(rs, hasItem(new Relation("E", "F")));
	}

	@Theory
	public void testAddAxioms(@RuleList List<Rule> rules) {
		InferenceEngine e = new InferenceEngine();
		e.applyRules(rules);
		Collection<PredicateValue> rs = e
				.addAxiomsTo(new ArrayList<PredicateValue>());
		assertThat(rs, hasSize(3));
		assertThat(rs, hasItem(new PredicateValue("A", FuzzyBoolean.UNKNOWN)));
		assertThat(rs, hasItem(new PredicateValue("B", FuzzyBoolean.UNKNOWN)));
		assertThat(rs, hasItem(new PredicateValue("C", FuzzyBoolean.UNKNOWN)));
	}

	@Theory
	public void testAnalyze(@RuleList List<Rule> rules, FuzzyBoolean va,
			FuzzyBoolean vb, FuzzyBoolean vc) {
		InferenceEngine e = new InferenceEngine();
		e.applyRules(rules);
		e.setPredicate("A", va);
		e.setPredicate("B", vb);
		e.setPredicate("C", vc);
		e.analyze();
		FuzzyBoolean vd = FuzzyBoolean.UNKNOWN.or(va).or(vb);
		FuzzyBoolean ve = FuzzyBoolean.UNKNOWN.or(vc);
		FuzzyBoolean vf = FuzzyBoolean.UNKNOWN.or(vd).or(ve);
		FuzzyBoolean vg = FuzzyBoolean.UNKNOWN.or(vc);
		String msg = va + ", " + vb + "," + vc;
		assertThat(e.pop(), nullValue());
		assertThat(msg, e.getValue("A"), equalTo(va));
		assertThat(msg, e.getValue("B"), equalTo(vb));
		assertThat(msg, e.getValue("C"), equalTo(vc));
		assertThat(msg, e.getValue("D"), equalTo(vd));
		assertThat(msg, e.getValue("E"), equalTo(ve));
		assertThat(msg, e.getValue("F"), equalTo(vf));
		assertThat(msg, e.getValue("G"), equalTo(vg));
	}

	@Theory
	public void testSetRules(@RuleList List<Rule> rules) {
		InferenceEngine e = new InferenceEngine();
		e.applyRules(rules);
		assertThat(e, hasProperty("axioms", containsInAnyOrder("A", "B", "C")));
		assertThat(e, hasProperty("hypothesis", containsInAnyOrder("F", "G")));
		assertThat(e, hasProperty("inferences", containsInAnyOrder("D", "E")));
		assertThat(
				e,
				hasProperty("predicates",
						containsInAnyOrder("A", "B", "C", "D", "E", "F", "G")));
	}

	@Theory
	public void testAddRelations(@RuleList List<Rule> rules) {
		InferenceEngine e = new InferenceEngine();
		e.applyRules(rules);
		FSet<Relation> rs = e.addRelations();
		assertThat(rs, hasSize(6));
		assertThat(rs, hasItem(new Relation("A", "D")));
		assertThat(rs, hasItem(new Relation("B", "D")));
		assertThat(rs, hasItem(new Relation("C", "E")));
		assertThat(rs, hasItem(new Relation("C", "G")));
		assertThat(rs, hasItem(new Relation("D", "F")));
		assertThat(rs, hasItem(new Relation("E", "F")));
	}

}
