package org.mmarini.fuzzy;

import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.util.HashSet;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

/**
 * @author US00852
 * @version $Id: RuleTest.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class RuleTest {
	static final String RULE_STRING = "if((A))then((B),not(C))else((D),not(E))";
	Rule applyRule;
	Rule seekRule;
	Rule stringRule;
	Rule rule1;
	Rule rule2;
	Constant ifExp;
	IAssignExpression thenExp;
	IAssignExpression elseExp;
	IPredicate aPredicate;
	IPredicate bPredicate;
	IPredicate cPredicate;
	IPredicate dPredicate;
	IPredicate ePredicate;
	IAssignExpression bExp;
	IAssignExpression cExp;
	IAssignExpression dExp;
	IAssignExpression eExp;
	FuzzyBoolean[][] values;
	IPredicate thenPredicate;
	IPredicate elsePredicate;
	IPredicate hypotesys;
	Set visited;

	/*
	 * @see TestCase#setUp()
	 */
	@Before
	public void init() throws Exception {
		values = new FuzzyBoolean[][] {
				{ FuzzyBoolean.FALSE, FuzzyBoolean.FALSE, FuzzyBoolean.FALSE,
						FuzzyBoolean.TRUE },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.QUITE_FALSE,
						FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.TRUE },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.UNKNOWN,
						FuzzyBoolean.UNKNOWN, FuzzyBoolean.TRUE },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.TRUE },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.TRUE, FuzzyBoolean.TRUE,
						FuzzyBoolean.TRUE },

				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.FALSE,
						FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_TRUE },
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_FALSE,
						FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_TRUE },
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.UNKNOWN,
						FuzzyBoolean.UNKNOWN, FuzzyBoolean.QUITE_TRUE },
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_TRUE },
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.TRUE,
						FuzzyBoolean.TRUE, FuzzyBoolean.TRUE },

				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.FALSE,
						FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.QUITE_FALSE,
						FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN,
						FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_TRUE },
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.TRUE, FuzzyBoolean.TRUE,
						FuzzyBoolean.TRUE },

				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.FALSE,
						FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_FALSE },
				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_FALSE,
						FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_FALSE },
				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.UNKNOWN,
						FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_TRUE },
				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.TRUE,
						FuzzyBoolean.TRUE, FuzzyBoolean.TRUE },

				{ FuzzyBoolean.TRUE, FuzzyBoolean.FALSE, FuzzyBoolean.TRUE,
						FuzzyBoolean.FALSE },
				{ FuzzyBoolean.TRUE, FuzzyBoolean.QUITE_FALSE,
						FuzzyBoolean.TRUE, FuzzyBoolean.QUITE_FALSE },
				{ FuzzyBoolean.TRUE, FuzzyBoolean.UNKNOWN, FuzzyBoolean.TRUE,
						FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.TRUE, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.TRUE, FuzzyBoolean.QUITE_TRUE },
				{ FuzzyBoolean.TRUE, FuzzyBoolean.TRUE, FuzzyBoolean.TRUE,
						FuzzyBoolean.TRUE }, };

		rule1 = new Rule();
		rule2 = new Rule();
		ifExp = new Constant();
		thenPredicate = bPredicate = new Predicate("B");
		elsePredicate = dPredicate = new Predicate("D");
		bExp = thenExp = new Assign(bPredicate);
		dExp = elseExp = new Assign(dPredicate);
		applyRule = new Rule();
		applyRule.setIfExpression(ifExp);
		applyRule.addThenExpression(thenExp);
		applyRule.addElseExpression(elseExp);

		aPredicate = new Predicate("A");
		cPredicate = new Predicate("C");
		ePredicate = new Predicate("E");
		stringRule = new Rule();
		stringRule.setIfExpression(new PredicateExpression(aPredicate));
		stringRule.addThenExpression(bExp);
		stringRule.addThenExpression(new NotAssign(cPredicate));
		stringRule.addElseExpression(dExp);
		stringRule.addElseExpression(new NotAssign(ePredicate));

		hypotesys = new Predicate("H");
		/*
		 * Seek rule: if(A)then((B),!(C))else((D),!(E))
		 */
		seekRule = stringRule;
	}

	@Test
	public void testAcceptElseIAssignVisitor() {
		final Set<IPredicate> visited = new HashSet<IPredicate>();
		stringRule.acceptElse(new IAssignVisitor() {

			@Override
			public void visitPredicate(IPredicate predicate) {
				visited.add(predicate);
			}

		});
		assertThat(visited, hasSize(2));
		assertThat(visited, hasItem(equalTo(dPredicate)));
		assertThat(visited, hasItem(equalTo(aPredicate)));
	}

	public void testAcceptThenIAssignVisitor() {
		stringRule.acceptThen(new IAssignVisitor() {

			@Override
			public void visitPredicate(IPredicate predicate) {
				visited.add(predicate);
			}

		});
		assertEquals(2, visited.size());
		assertTrue(visited.contains(bPredicate));
		assertTrue(visited.contains(cPredicate));
	}

	public void testAddElseExpression() {
		assertEquals(0, rule1.getElseExpression().size());
		rule1.addElseExpression(thenExp);
		assertEquals(1, rule1.getElseExpression().size());
		assertSame(thenExp, rule1.getElseExpression().get(0));
	}

	public void testAddThenExpression() {
		assertEquals(0, rule1.getThenExpression().size());
		rule1.addThenExpression(thenExp);
		assertEquals(1, rule1.getThenExpression().size());
		assertSame(thenExp, rule1.getThenExpression().get(0));
	}

	public void testApply() {
		for (int i = 0; i < values.length; ++i) {
			applyRule.reset();
			assertEquals(false, applyRule.isApplied());
			ifExp.setValue(values[i][0]);
			thenPredicate.setValue(values[i][1]);
			elsePredicate.setValue(values[i][1]);
			applyRule.apply(null);
			assertEquals(true, applyRule.isApplied());
			assertEquals(values[i][0] + "," + values[i][1], values[i][2],
					thenPredicate.getValue());
			assertEquals(values[i][0] + "," + values[i][1], values[i][3],
					elsePredicate.getValue());
		}
	}

	public void testAssigns() {
		assertFalse(stringRule.assigns(aPredicate));
		assertTrue(stringRule.assigns(bPredicate));
		assertTrue(stringRule.assigns(cPredicate));
		assertTrue(stringRule.assigns(dPredicate));
		assertTrue(stringRule.assigns(ePredicate));
	}

	public void testEquals() {
		assertTrue(rule1.equals(rule1));
		assertFalse(rule1.equals(null));
		assertFalse(rule1.equals(new Object()));

		assertTrue(rule1.equals(rule2));
		assertTrue(rule2.equals(rule1));
		assertTrue(rule1.hashCode() == rule2.hashCode());

		rule1.setIfExpression(ifExp);
		assertFalse(rule1.equals(rule2));
		assertFalse(rule2.equals(rule1));

		rule2.setIfExpression(ifExp);
		assertTrue(rule1.equals(rule2));
		assertTrue(rule2.equals(rule1));
		assertTrue(rule1.hashCode() == rule2.hashCode());

		rule1.addThenExpression(thenExp);
		assertFalse(rule1.equals(rule2));
		assertFalse(rule2.equals(rule1));

		rule2.addThenExpression(thenExp);
		assertTrue(rule1.equals(rule2));
		assertTrue(rule2.equals(rule1));
		assertTrue(rule1.hashCode() == rule2.hashCode());

		rule1.addElseExpression(thenExp);
		assertFalse(rule1.equals(rule2));
		assertFalse(rule2.equals(rule1));

		rule2.addElseExpression(thenExp);
		assertTrue(rule1.equals(rule2));
		assertTrue(rule2.equals(rule1));
		assertTrue(rule1.hashCode() == rule2.hashCode());

		rule1.setApplied(true);
		assertFalse(rule1.equals(rule2));
		assertFalse(rule2.equals(rule1));

		rule2.setApplied(true);
		assertTrue(rule1.equals(rule2));
		assertTrue(rule2.equals(rule1));
		assertTrue(rule1.hashCode() == rule2.hashCode());
	}

	public void testSeekForPostulate() {
		IEvaluateContext evalCtx = new IEvaluateContext() {
			@Override
			public FuzzyBoolean evaluate(IPredicate predicate) {
				return predicate.getValue();
			}
		};

		seekRule.apply(evalCtx);

		WeightCalculator calc = new WeightCalculator(new IScanContext() {

			@Override
			public void scan(IWeightContext context, IPredicate predicate) {
				context.addPostulate(predicate);
			}
		});

		calc.startHypotesys(bPredicate);
		seekRule.resetWeighted();
		seekRule.seekForPostulate(calc, bPredicate);
		assertEquals(new Weight(1, 0), calc.getWeight(aPredicate));

		calc.startHypotesys(cPredicate);
		seekRule.resetWeighted();
		seekRule.seekForPostulate(calc, cPredicate);
		assertEquals(new Weight(2, 0), calc.getWeight(aPredicate));

		calc.startHypotesys(dPredicate);
		seekRule.resetWeighted();
		seekRule.seekForPostulate(calc, dPredicate);
		assertEquals(new Weight(3, 0), calc.getWeight(aPredicate));

		calc.startHypotesys(ePredicate);
		seekRule.resetWeighted();
		seekRule.seekForPostulate(calc, ePredicate);
		assertEquals(new Weight(4, 0), calc.getWeight(aPredicate));
	}

	public void testSetIfExpression() {
		assertNull(rule1.getIfExpression());
		rule1.setIfExpression(ifExp);
		assertSame(ifExp, rule1.getIfExpression());
	}

	/**
	 * 
	 */
	public void testToString() {
		assertEquals(RULE_STRING, stringRule.toString());
	}
}