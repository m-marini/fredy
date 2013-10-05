/**
 * 
 */
package org.mmarini.fuzzy;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;

import org.mmarini.fuzzy.parse.RulesParser;
import org.xml.sax.SAXException;

/**
 * @author US00852
 * 
 */
public class InferenceEngine implements ExecutionContext {
	private Queue<FuzzyBoolean> stack;
	private Map<String, FuzzyBoolean> predicateValues;
	private Set<String> hypothesis;
	private Set<String> predicates;
	private Set<String> inferences;
	private Set<String> axioms;
	private List<Rule> rules;
	private Set<Rule> analysisRules;
	private Set<String> unknownPredicate;

	/**
	 * 
	 */
	public InferenceEngine() {
		predicateValues = new HashMap<String, FuzzyBoolean>();
		stack = Collections.asLifoQueue(new ArrayDeque<FuzzyBoolean>());
		hypothesis = new HashSet<String>();
		predicates = new HashSet<String>();
		axioms = new HashSet<String>();
		inferences = new HashSet<String>();
		analysisRules = new HashSet<Rule>();
		unknownPredicate = new HashSet<String>();
	}

	/**
	 * 
	 * @param url
	 * 
	 * @throws IOException
	 * @throws SAXException
	 * @throws ParserConfigurationException
	 * @throws FactoryConfigurationError
	 */
	public void loadRules(URL url) throws FactoryConfigurationError,
			ParserConfigurationException, SAXException, IOException {
		setRules(RulesParser.getInstance().parse(url));
	}

	/**
	 * Analyze all hypothesis
	 */
	public void analyze() {
		analysisRules.addAll(rules);
		unknownPredicate.addAll(hypothesis);
		unknownPredicate.addAll(inferences);
		for (String p : unknownPredicate) {
			predicateValues.remove(p);
		}
		for (String h : hypothesis) {
			infer(h);
		}
	}

	/**
	 * 
	 * @param status
	 */
	public void applyStatus(Map<String, FuzzyBoolean> status) {
		predicateValues.clear();
		predicateValues.putAll(status);
	}

	/**
	 * @see org.mmarini.fuzzy.ExecutionContext#assignFalse(java.lang.String)
	 */
	@Override
	public void assignFalse(String predicate) {
		FuzzyBoolean value = pop().and(getValue(predicate));
		predicateValues.put(predicate, value);
	}

	/**
	 * @see org.mmarini.fuzzy.ExecutionContext#assignTrue(java.lang.String)
	 */
	@Override
	public void assignTrue(String predicate) {
		FuzzyBoolean value = pop().or(getValue(predicate));
		predicateValues.put(predicate, value);
	}

	/**
	 * @return the axioms
	 */
	public Set<String> getAxioms() {
		return axioms;
	}

	/**
	 * @return the hypotesys
	 */
	public Set<String> getHypothesis() {
		return hypothesis;
	}

	/**
	 * @return the inferences
	 */
	public Set<String> getInferences() {
		return inferences;
	}

	/**
	 * @return the predicates
	 */
	public Set<String> getPredicates() {
		return predicates;
	}

	/**
	 * @return the rules
	 */
	public List<Rule> getRules() {
		return rules;
	}

	/**
	 * 
	 * @param predicate
	 * @return
	 */
	public FuzzyBoolean getValue(String predicate) {
		FuzzyBoolean value = predicateValues.get(predicate);
		return value != null ? value : FuzzyBoolean.UNKNOWN;
	}

	/**
	 * 
	 * @param p
	 */
	private void infer(String p) {
		if (unknownPredicate.contains(p)) {
			unknownPredicate.remove(p);
			for (Rule r : new ArrayList<Rule>(analysisRules)) {
				if (analysisRules.contains(r) && r.hasConsequence(p)) {
					analysisRules.remove(r);
					for (String c : r.mapToCondition(new HashSet<String>())) {
						infer(c);
					}
					r.execute(this);
				}
			}
		}
	}

	/**
	 * Parse the rules to find the predicates, hypothesis, axioms and
	 * inferences.
	 * <p>
	 * Given X the condition set and Y the consequence set we have:
	 * <ul>
	 * <li>the predicates are P = X &#x222A; Y</li>
	 * <li>the axioms are A = X - Y</li>
	 * <li>the hypothesis are H = Y - X</li>
	 * <li>the inferences are I = Y - H</li>
	 * </ul>
	 * </p>
	 */
	private void parseRules() {
		predicates.clear();
		hypothesis.clear();
		axioms.clear();
		inferences.clear();

		for (Rule r : rules) {
			r.mapToInference(inferences);
			r.mapToCondition(axioms);
		}

		predicates.addAll(axioms);
		predicates.addAll(inferences);

		hypothesis.addAll(inferences);
		hypothesis.removeAll(axioms);

		axioms.removeAll(inferences);
		inferences.removeAll(hypothesis);
	}

	/**
	 * @see org.mmarini.fuzzy.ExecutionContext#pop()
	 */
	@Override
	public FuzzyBoolean pop() {
		return stack.poll();
	}

	/**
	 * @see org.mmarini.fuzzy.ExecutionContext#push(org.mmarini.fuzzy.FuzzyBoolean)
	 */
	@Override
	public void push(FuzzyBoolean value) {
		stack.offer(value);
	}

	/**
	 * @see org.mmarini.fuzzy.ExecutionContext#push(java.lang.String)
	 */
	@Override
	public void push(String predicate) {
		push(getValue(predicate));
	}

	/**
	 * 
	 * @param list
	 * @return
	 */
	public Collection<PredicateValue> addAxiomsTo(
			Collection<PredicateValue> list) {
		return addValuesTo(list, axioms);
	}

	/**
	 * 
	 * @param list
	 * @return
	 */
	public Collection<PredicateValue> addPredicatesTo(
			Collection<PredicateValue> list) {
		return addValuesTo(list, predicates);
	}

	/**
	 * 
	 * @param list
	 * @return
	 */
	public Collection<PredicateValue> addHypothesisTo(
			Collection<PredicateValue> list) {
		return addValuesTo(list, hypothesis);
	}

	/**
	 * 
	 * @param list
	 * @return
	 */
	public Collection<PredicateValue> addInferencesTo(
			Collection<PredicateValue> list) {
		return addValuesTo(list, inferences);
	}

	/**
	 * 
	 * @param list
	 * @param names
	 * @return
	 */
	private Collection<PredicateValue> addValuesTo(
			Collection<PredicateValue> list, Collection<String> names) {
		for (String n : names) {
			list.add(new PredicateValue(n, getValue(n)));
		}
		return list;
	}

	/**
	 * 
	 * @param predicate
	 * @param value
	 */
	public void setPredicate(String predicate, FuzzyBoolean value) {
		predicateValues.put(predicate, value);
	}

	/**
	 * @param rules
	 *            the rules to set
	 */
	public void setRules(List<Rule> rules) {
		this.rules = rules;
		parseRules();

	}
}
