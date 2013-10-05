package org.mmarini.fuzzy;

import java.util.HashSet;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;
import static org.hamcrest.CoreMatchers.*;

/**
 * @author US00852
 * @version $Id: AbstractExpressionTest.java,v 1.2 2005/02/10 22:32:35 marco Exp
 *          $
 */
public class AbstractExpressionTest {
	AbstractExpression equalsExpression1;
	AbstractExpression equalsExpression2;
	AbstractExpression acceptExpression;
	AbstractExpression evalExpression;
	protected AbstractExpression scanExp;
	Predicate[] predicates;
	Predicate hypotesys;
	PredicateExpression[] predicateExps;
	String expectedString;
	Set<IPredicate> predicateVisited;
	Object[][][] scanValues;
	IEvaluateContext evalContext;

	@Before
	public void init() throws Exception {
		predicateVisited = new HashSet<IPredicate>();
		hypotesys = new Predicate("H");
		evalContext = new IEvaluateContext() {

			@Override
			public FuzzyBoolean evaluate(IPredicate predicate) {
				return predicate.getValue();
			}
		};
	}

	@Test
	public void testAcceptIExpressionVisitor() {
		acceptExpression.accept(new IExpressionVisitor() {

			@Override
			public void visitPredicate(IPredicate predicate) {
				predicateVisited.add(predicate);
			}
		});
		assertEquals(predicates.length, predicateVisited.size());
	}

	/*
	 * Class under test for boolean equals(Object)
	 */
	@Test
	public void testEqualsObject() {
		assertTrue(equalsExpression1.equals(equalsExpression1));
		assertFalse(equalsExpression1.equals(null));
		assertFalse(equalsExpression1.equals(new Object()));

		assertTrue(equalsExpression1.equals(equalsExpression2));
		assertTrue(equalsExpression2.equals(equalsExpression1));
		assertTrue(equalsExpression1.hashCode() == equalsExpression2.hashCode());
	}

	@Test
	public void testScan() {
		for (int i = 0; i < scanValues.length; ++i) {
			String testName = "Test " + i;
			for (int j = 0; j < predicates.length; ++j) {
				predicates[j].setValue((FuzzyBoolean) scanValues[i][0][j]);
			}
			scanExp.reset();
			scanExp.evaluate(evalContext);
			WeightCalculator calc = new WeightCalculator(new IScanContext() {

				@Override
				public void scan(IWeightContext context, IPredicate predicate) {
					context.addPostulate(predicate);
				}
			});

			calc.startHypotesys(hypotesys);
			scanExp.scan(calc);
			for (int j = 0; j < predicates.length; ++j) {
				assertEquals(testName + " " + predicates[j].toString(),
						scanValues[i][1][j], calc.getWeight(predicates[j]));
			}
		}
	}

	@Test
	public void testToString() {
		assertThat(acceptExpression.toString(), containsString(expectedString));
	}
}