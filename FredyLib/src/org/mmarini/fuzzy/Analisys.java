package org.mmarini.fuzzy;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author US00852
 * @version $Id: Analisys.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class Analisys implements IEvaluateContext, IScanContext, Serializable {
	private Map predicate = new HashMap();
	private List rule = new ArrayList();
	private List postulate = new ArrayList();
	private List hypotesys = new ArrayList();
	private Set evidence = new HashSet();

	/**
	 * 
	 */
	public Analisys() {
	}

	/**
	 * Adds a predicate. It creates a new predicate if it does not exist
	 * 
	 * @param name
	 *            the name of predicate
	 * @return the predicate
	 */
	public IPredicate addPredicate(String name) {
		IPredicate predicate = this.getPredicate(name);
		if (predicate == null) {
			predicate = new Predicate(name);
			this.getPredicate().put(name, predicate);
			this.getPostulate().add(predicate);
			this.getHypotesys().add(predicate);
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
		this.getRule().add(rule);
		this.removeFromHypotesys(rule);
		this.removeFromPostulate(rule);
		this.addToEvidence(rule);
	}

	/**
	 * Removes from postulate list all of then-else expression predicates
	 * 
	 * @param rule
	 *            the rules
	 */
	protected void addToEvidence(Rule rule) {
		IAssignVisitor assignVisitor = new IAssignVisitor() {

			@Override
			public void visitPredicate(IPredicate predicate) {
				getEvidence().add(predicate);
			}

		};
		rule.acceptThen(assignVisitor);
		rule.acceptElse(assignVisitor);
	}

	/**
	 * Analizes the hypotesis
	 */
	public void analize() {
		this.reset();
		for (Iterator iter = this.getHypotesys().iterator(); iter.hasNext();) {
			this.evaluate((IPredicate) iter.next());
		}
		this.calculateWeights();
	}

	protected void calculateWeights() {
		WeightCalculator calculator = new WeightCalculator(this);
		for (Iterator iter = this.getHypotesys().iterator(); iter.hasNext();) {
			for (Iterator iter1 = this.getRule().iterator(); iter1.hasNext();) {
				Rule rule = (Rule) iter1.next();
				rule.resetWeighted();
			}
			IPredicate hypotesys = (IPredicate) iter.next();
			calculator.startHypotesys(hypotesys);
			calculator.scan(calculator, hypotesys);
		}
		/*
		 * Sets the weights
		 */
		for (Iterator iter = this.getPostulate().iterator(); iter.hasNext();) {
			IPredicate postulate = (IPredicate) iter.next();
			Weight weight = calculator.getWeight(postulate);
			postulate.setWeight(weight);
		}
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object other) {
		if (this == other)
			return true;
		if (other == null)
			return false;
		if (!(other instanceof Analisys))
			return false;
		Analisys analisys = (Analisys) other;
		if (!(this.getRule().equals(analisys.getRule())))
			return false;
		if (!(this.getPredicate().equals(analisys.getPredicate())))
			return false;
		return true;
	}

	/**
	 * @see org.mmarini.fuzzy.IEvaluateContext#evaluate(org.mmarini.fuzzy.IPredicate)
	 */
	@Override
	public FuzzyBoolean evaluate(IPredicate predicate) {
		for (Iterator iter = this.getRule().iterator(); iter.hasNext();) {
			Rule rule = (Rule) iter.next();
			if (rule.assigns(predicate))
				rule.apply(this);
		}
		return predicate.getValue();
	}

	/**
	 * Returns the evidence.
	 * 
	 * @return the evidence.
	 */
	public Set getEvidence() {
		return evidence;
	}

	/**
	 * Returns the list of hypotesys
	 * 
	 * @return the list of hypotesys
	 */
	public List getHypotesys() {
		return hypotesys;
	}

	/**
	 * Returns the list of postulates
	 * 
	 * @return the list of postulates
	 */
	public List getPostulate() {
		return postulate;
	}

	/**
	 * Returns the predicate list
	 * 
	 * @return the predicate list
	 */
	protected Map getPredicate() {
		return predicate;
	}

	/**
	 * Returns a predicate
	 * 
	 * @param name
	 *            the name of predicate
	 * @return the predicate or null if it does not exit
	 */
	public IPredicate getPredicate(String name) {
		return (IPredicate) this.getPredicate().get(name);
	}

	/**
	 * Returns the rule list
	 * 
	 * @return the rule list.
	 */
	public List getRule() {
		return rule;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return this.getRule().hashCode() * 31 + this.getPredicate().hashCode();
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
				getHypotesys().remove(predicate);
			}
		});
	}

	/**
	 * Removes from postulate list all of then-else expression predicates
	 * 
	 * @param rule
	 *            the rules
	 */
	protected void removeFromPostulate(Rule rule) {
		IAssignVisitor assignVisitor = new IAssignVisitor() {

			@Override
			public void visitPredicate(IPredicate predicate) {
				getPostulate().remove(predicate);
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
		for (Iterator iter = this.getRule().iterator(); iter.hasNext();) {
			((Rule) iter.next()).reset();
		}
		// Reset all evidence
		for (Iterator iter = this.getEvidence().iterator(); iter.hasNext();) {
			IPredicate predicate = (IPredicate) iter.next();
			predicate.reset();
		}
	}

	@Override
	public void scan(IWeightContext context, IPredicate evidence) {
		if (evidence.getValue().isKnown())
			return;
		if (this.getPostulate().contains(evidence)) {
			context.addPostulate(evidence);
		} else {
			for (Iterator iter = this.getRule().iterator(); iter.hasNext();) {
				Rule rule = (Rule) iter.next();
				rule.seekForPostulate(context, evidence);
			}
		}
	}
}