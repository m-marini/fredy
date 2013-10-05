package org.mmarini.fuzzy;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author US00852
 * @version $Id: Analisys.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class Analisys implements IEvaluateContext, IScanContext, Serializable {
	private static final long serialVersionUID = -5543477823656335703L;
	private Map<String, IPredicate> predicate;
	private List<Rule> rule;
	private List<IPredicate> postulate;
	private List<IPredicate> hypotesys;
	private Set<IPredicate> evidence;

	/**
	 * 
	 */
	public Analisys() {
		predicate = new HashMap<String, IPredicate>();
		rule = new ArrayList<Rule>();
		postulate = new ArrayList<IPredicate>();
		hypotesys = new ArrayList<IPredicate>();
		evidence = new HashSet<IPredicate>();
	}

	/**
	 * Adds a predicate. It creates a new predicate if it does not exist
	 * 
	 * @param name
	 *            the name of predicate
	 * @return the predicate
	 */
	public IPredicate addPredicate(String name) {
		IPredicate predicate = this.predicate.get(name);
		if (predicate == null) {
			predicate = new Predicate(name);
			this.predicate.put(name, predicate);
			postulate.add(predicate);
			hypotesys.add(predicate);
		}
		return predicate;
	}

	/**
	 * Adds a rule
	 * 
	 * @param rule
	 *            the rule
	 */
	public void addRule(Rule rule) {
		this.rule.add(rule);
		removeFromHypotesys(rule);
		removeFromPostulate(rule);
		addToEvidence(rule);
	}

	/**
	 * Removes from postulate list all of then-else expression predicates
	 * 
	 * @param rule
	 *            the rules
	 */
	private void addToEvidence(Rule rule) {
		IAssignVisitor assignVisitor = new IAssignVisitor() {

			@Override
			public void visitPredicate(IPredicate predicate) {
				evidence.add(predicate);
			}

		};
		rule.acceptThen(assignVisitor);
		rule.acceptElse(assignVisitor);
	}

	/**
	 * Analizes the hypotesis
	 */
	public void analize() {
		reset();
		for (IPredicate iter : hypotesys) {
			evaluate(iter);
		}
		calculateWeights();
	}

	/**
	 * 
	 */
	private void calculateWeights() {
		WeightCalculator calculator = new WeightCalculator(this);
		for (IPredicate h : hypotesys) {
			for (Rule r : this.rule) {
				r.resetWeighted();
			}
			calculator.startHypotesys(h);
			calculator.scan(calculator, h);
		}
		/*
		 * Sets the weights
		 */
		for (IPredicate p : postulate) {
			Weight weight = calculator.getWeight(p);
			p.setWeight(weight);
		}
	}

	/**
	 * @see org.mmarini.fuzzy.IEvaluateContext#evaluate(org.mmarini.fuzzy.IPredicate)
	 */
	@Override
	public FuzzyBoolean evaluate(IPredicate predicate) {
		for (Rule r : this.rule) {
			if (r.assigns(predicate))
				r.apply(this);
		}
		return predicate.getValue();
	}

	/**
	 * Returns the evidence.
	 * 
	 * @return the evidence.
	 */
	public Set<IPredicate> getEvidence() {
		return evidence;
	}

	/**
	 * Returns the list of hypotesys
	 * 
	 * @return the list of hypotesys
	 */
	public List<IPredicate> getHypotesys() {
		return hypotesys;
	}

	/**
	 * Returns the list of postulates
	 * 
	 * @return the list of postulates
	 */
	public List<IPredicate> getPostulate() {
		return postulate;
	}

	/**
	 * Returns a predicate
	 * 
	 * @param name
	 *            the name of predicate
	 * @return the predicate or null if it does not exit
	 */
	public IPredicate getPredicate(String name) {
		return predicate.get(name);
	}

	/**
	 * Returns the rule list
	 * 
	 * @return the rule list.
	 */
	public List<Rule> getRule() {
		return rule;
	}

	/**
	 * Removes from hypotesys list all of if expression predicates
	 * 
	 * @param rule
	 */
	protected void removeFromHypotesys(Rule rule) {
		rule.getIfExpression().accept(new IExpressionVisitor() {

			@Override
			public void visitPredicate(IPredicate predicate) {
				hypotesys.remove(predicate);
			}
		});
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((evidence == null) ? 0 : evidence.hashCode());
		result = prime * result
				+ ((hypotesys == null) ? 0 : hypotesys.hashCode());
		result = prime * result
				+ ((postulate == null) ? 0 : postulate.hashCode());
		result = prime * result
				+ ((predicate == null) ? 0 : predicate.hashCode());
		result = prime * result + ((rule == null) ? 0 : rule.hashCode());
		return result;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Analisys other = (Analisys) obj;
		if (evidence == null) {
			if (other.evidence != null)
				return false;
		} else if (!evidence.equals(other.evidence))
			return false;
		if (hypotesys == null) {
			if (other.hypotesys != null)
				return false;
		} else if (!hypotesys.equals(other.hypotesys))
			return false;
		if (postulate == null) {
			if (other.postulate != null)
				return false;
		} else if (!postulate.equals(other.postulate))
			return false;
		if (predicate == null) {
			if (other.predicate != null)
				return false;
		} else if (!predicate.equals(other.predicate))
			return false;
		if (rule == null) {
			if (other.rule != null)
				return false;
		} else if (!rule.equals(other.rule))
			return false;
		return true;
	}

	/**
	 * Removes from postulate list all of then-else expression predicates
	 * 
	 * @param rule
	 *            the rules
	 */
	private void removeFromPostulate(Rule rule) {
		IAssignVisitor assignVisitor = new IAssignVisitor() {

			@Override
			public void visitPredicate(IPredicate predicate) {
				postulate.remove(predicate);
			}

		};
		rule.acceptThen(assignVisitor);
		rule.acceptElse(assignVisitor);
	}

	/**
	 * Resets the analisys
	 */
	public void reset() {
		// Reset all rules
		for (Rule r : this.rule) {
			r.reset();
		}
		// Reset all evidence
		for (IPredicate p : evidence) {
			p.reset();
		}
	}

	/**
	 * 
	 */
	@Override
	public void scan(IWeightContext context, IPredicate evidence) {
		if (evidence.getValue().isKnown())
			return;
		if (postulate.contains(evidence)) {
			context.addPostulate(evidence);
		} else {
			for (Rule r : rule) {
				r.seekForPostulate(context, evidence);
			}
		}
	}
}