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
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;

import org.mmarini.functional.FEntry;
import org.mmarini.functional.FList;
import org.mmarini.functional.FMap;
import org.mmarini.functional.FSet;
import org.mmarini.functional.Functor1;
import org.mmarini.functional.Functor2;
import org.mmarini.functional.ListImpl;
import org.mmarini.functional.MapImpl;
import org.mmarini.functional.SetImpl;
import org.mmarini.fuzzy.parse.RulesParser;
import org.xml.sax.SAXException;

/**
 * @author US00852
 * 
 */
public class InferenceEngine implements ExecutionContext {
	private Queue<FuzzyBoolean> stack;
	private FMap<String, FuzzyBoolean> predicateValues;
	private FSet<String> hypothesis;
	private FSet<String> predicates;
	private FSet<String> inferences;
	private FSet<String> axioms;
	private FList<Rule> rules;
	private Set<Rule> analysisRules;
	private Set<String> unknownPredicate;
	private FList<AxiomInfo> axiomInfos;
	private Functor1<PredicateValue, String> predicateValueBuilder;

	/**
	 * 
	 */
	public InferenceEngine() {
		predicateValues = new MapImpl<String, FuzzyBoolean>();
		stack = Collections.asLifoQueue(new ArrayDeque<FuzzyBoolean>());
		hypothesis = new SetImpl<String>();
		predicates = new SetImpl<String>();
		axioms = new SetImpl<String>();
		inferences = new SetImpl<String>();
		analysisRules = new HashSet<Rule>();
		unknownPredicate = new HashSet<String>();
		rules = new ListImpl<Rule>();
		axiomInfos = new ListImpl<AxiomInfo>();
		predicateValueBuilder = new Functor1<PredicateValue, String>() {

			@Override
			public PredicateValue apply(String p) {
				return new PredicateValue(p, getValue(p));
			}

		};
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
	 * @param list
	 */
	public Collection<? extends PredicateValue> applyPredicates(
			Collection<? extends PredicateValue> list) {
		for (PredicateValue p : list) {
			predicateValues.put(p.getPredicate(), p.getValue());
		}
		return list;
	}

	/**
	 * @param rules
	 *            the rules to set
	 */
	public void applyRules(List<Rule> rules) {
		this.rules.clear();
		this.rules.addAll(rules);
		parseRules();
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
	 * @see org.mmarini.fuzzy.ExecutionContext#assertFalse(java.lang.String)
	 */
	@Override
	public void assertFalse(String predicate) {
		FuzzyBoolean value = pop().not().and(getValue(predicate));
		predicateValues.put(predicate, value);
	}

	/**
	 * @see org.mmarini.fuzzy.ExecutionContext#assertTrue(java.lang.String)
	 */
	@Override
	public void assertTrue(String predicate) {
		FuzzyBoolean value = pop().or(getValue(predicate));
		predicateValues.put(predicate, value);
	}

	/**
	 * 
	 * @param rels
	 * @return
	 */
	private FSet<Relation> computeClosure(FSet<Relation> rels) {
		Set<String> sources = rels.map(Relation.sourceGetter);
		Set<String> targets = rels.map(Relation.targetGetter);

		SetImpl<Relation> c = new SetImpl<Relation>(rels);
		for (String s : sources) {
			for (String t : targets) {
				Relation r = new Relation(s, t);
				if (!s.equals(t) && !c.contains(r)) {
					for (String w : targets) {
						if (!w.equals(s) && !w.equals(t)
								&& c.contains(new Relation(s, w))
								&& c.contains(new Relation(w, t))) {
							c.add(r);
						}
					}
				}
			}
		}
		return c;
	}

	/**
	 * 
	 * @param closure
	 * @return
	 */
	private FList<AxiomInfo> computePredicateInfos(FSet<Relation> closure) {
		closure = closure.filter(new Functor1<Boolean, Relation>() {

			@Override
			public Boolean apply(Relation p) {
				return axioms.contains(p.getSource())
						&& hypothesis.contains(p.getTarget());
			}
		});

		final FMap<String, FSet<String>> ax = closure
				.groupBy(Relation.sourceGetter)
				.map(new Functor2<FEntry<String, FSet<String>>, String, FSet<Relation>>() {

					@Override
					public FEntry<String, FSet<String>> apply(String a,
							FSet<Relation> h) {
						FSet<String> v = h.map(Relation.targetGetter);
						return new FEntry<String, FSet<String>>(a, v);
					}
				});
		final FMap<String, Integer> hy = closure
				.groupBy(Relation.targetGetter)
				.map(new Functor2<FEntry<String, Integer>, String, FSet<Relation>>() {
					@Override
					public FEntry<String, Integer> apply(String p1,
							FSet<Relation> p2) {
						return new FEntry<String, Integer>(p1, p2.size());
					}
				});
		return ax.mapToList(new Functor2<AxiomInfo, String, FSet<String>>() {

			@Override
			public AxiomInfo apply(String a, FSet<String> hl) {
				return new AxiomInfo(a, hl.map(new Functor1<Integer, String>() {

					@Override
					public Integer apply(String h) {
						return hy.get(h);
					}
				}).min(new Comparator<Integer>() {

					@Override
					public int compare(Integer o1, Integer o2) {
						return o1.compareTo(o2);
					}
				}), hl.size());
			}
		});
	}

	/**
	 * @return the axioms
	 */
	public FSet<String> getAxioms() {
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
	 * @return the predicateInfos
	 */
	public FList<AxiomInfo> getPredicateInfos() {
		return axiomInfos;
	}

	/**
	 * @return the predicates
	 */
	public Set<String> getPredicates() {
		return predicates;
	}

	/**
	 * 
	 * @param relations
	 * @return
	 */
	FSet<Relation> getRelations() {
		FSet<Relation> relations = new SetImpl<Relation>();
		for (Rule r : rules)
			r.addRelations(relations);
		return relations;
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
					for (String c : r.retrieveCondition()) {
						infer(c);
					}
					r.execute(this);
				}
			}
		}
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
		applyRules(RulesParser.getInstance().parse(url));
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
		FSet<Relation> rels = getRelations();
		FSet<String> x = rels.map(Relation.sourceGetter);
		FSet<String> y = rels.map(Relation.targetGetter);

		predicates = x.union(y);
		axioms = x.notIn(y);
		hypothesis = y.notIn(x);
		inferences = y.notIn(hypothesis);
		axiomInfos = computePredicateInfos(computeClosure(rels));
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
	public FList<AxiomValue> retreiveAxiomValues() {
		return axiomInfos.map(new Functor1<AxiomValue, AxiomInfo>() {

			@Override
			public AxiomValue apply(AxiomInfo p) {
				return new AxiomValue(p, getValue(p.getPredicate()));
			}
		});
	}

	/**
	 * 
	 * @param list
	 * @return
	 */
	public FList<PredicateValue> retrieveHypothesisValues() {
		return hypothesis.mapToList(predicateValueBuilder);
	}

	/**
	 * 
	 * @param list
	 * @return
	 */
	public FList<PredicateValue> retrieveInferenceValues() {
		return inferences.mapToList(predicateValueBuilder);
	}

	/**
	 * 
	 * @param list
	 * @return
	 */
	public FList<PredicateValue> retrievePredicateValues() {
		return predicates.mapToList(predicateValueBuilder);
	}

	/**
	 * 
	 * @param predicate
	 * @param value
	 */
	public void setPredicate(String predicate, FuzzyBoolean value) {
		predicateValues.put(predicate, value);
	}
}
