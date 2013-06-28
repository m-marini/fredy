package org.mmarini.fuzzy;

import java.util.HashSet;
import java.util.Set;

import org.mmarini.fuzzy.AbstractExpression;
import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.IEvaluateContext;
import org.mmarini.fuzzy.IExpressionVisitor;
import org.mmarini.fuzzy.IPredicate;
import org.mmarini.fuzzy.IScanContext;
import org.mmarini.fuzzy.IWeightContext;
import org.mmarini.fuzzy.Predicate;
import org.mmarini.fuzzy.PredicateExpression;
import org.mmarini.fuzzy.WeightCalculator;

import junit.framework.TestCase;

/**
 * @author US00852
 * @version $Id: AbstractExpressionTest.java,v 1.2 2005/02/10 22:32:35 marco Exp
 *          $
 */
public class AbstractExpressionTest extends TestCase {
	AbstractExpression equalsExpression1;
	AbstractExpression equalsExpression2;
	AbstractExpression acceptExpression;
	AbstractExpression evalExpression;
	protected AbstractExpression scanExp;
	Predicate[] predicates;
	Predicate hypotesys;
	PredicateExpression[] predicateExps;
	String expectedString;
	Set predicateVisited = new HashSet();
	Object[][][] scanValues;
	IEvaluateContext evalContext;

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		hypotesys = new Predicate("H");
		evalContext = new IEvaluateContext() {

			@Override
			public FuzzyBoolean evaluate(IPredicate predicate) {
				return predicate.getValue();
			}
		};
	}

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
	public void testEqualsObject() {
		assertTrue(equalsExpression1.equals(equalsExpression1));
		assertFalse(equalsExpression1.equals(null));
		assertFalse(equalsExpression1.equals(new Object()));

		assertTrue(equalsExpression1.equals(equalsExpression2));
		assertTrue(equalsExpression2.equals(equalsExpression1));
		assertTrue(equalsExpression1.hashCode() == equalsExpression2.hashCode());
	}

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

	public void testToString() {
		assertEquals(expectedString, acceptExpression.toString());
	}
}