package org.mmarini.fuzzy;

import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.IEvaluateContext;
import org.mmarini.fuzzy.IExpressionVisitor;
import org.mmarini.fuzzy.IPredicate;
import org.mmarini.fuzzy.IScanContext;
import org.mmarini.fuzzy.IWeightContext;
import org.mmarini.fuzzy.Predicate;
import org.mmarini.fuzzy.PredicateExpression;
import org.mmarini.fuzzy.Weight;
import org.mmarini.fuzzy.WeightCalculator;

import junit.framework.TestCase;

/**
 * @author US00852
 * @version $Id: PredicateExpressionTest.java,v 1.1 2004/11/27 10:35:56 marco
 *          Exp $
 */
public class PredicateExpressionTest extends TestCase {
	private static final Weight W10 = new Weight(1, 0);

	private Predicate predicate1;
	private Predicate predicate2;
	private Predicate hypotesys;
	private PredicateExpression equExp1;
	private PredicateExpression equExp2;
	private boolean visitorCalled;
	private IEvaluateContext ctx;
	private WeightCalculator context;

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		predicate1 = new Predicate("A");
		predicate2 = new Predicate("B");
		hypotesys = new Predicate("H");
		equExp1 = new PredicateExpression(predicate1);
		equExp2 = new PredicateExpression(predicate1);
		visitorCalled = false;
		ctx = new IEvaluateContext() {

			@Override
			public FuzzyBoolean evaluate(IPredicate predicate) {
				return predicate.getValue();
			}
		};
		context = new WeightCalculator(new IScanContext() {

			@Override
			public void scan(IWeightContext context, IPredicate predicate) {
				context.addPostulate(predicate);
			}

		});
	}

	public void testAccept() {
		equExp1.accept(new IExpressionVisitor() {

			@Override
			public void visitPredicate(IPredicate predicate) {
				assertEquals(predicate1, predicate);
				visitorCalled = true;
			}
		});
		assertTrue(visitorCalled);
	}

	public void testEquals() {
		assertTrue(equExp1.equals(equExp1));
		assertFalse(equExp1.equals(null));
		assertFalse(equExp1.equals(new Object()));

		assertTrue(equExp1.equals(equExp2));
		assertTrue(equExp2.equals(equExp1));
		assertTrue(equExp1.hashCode() == equExp2.hashCode());

		equExp1.setPredicate(predicate2);
		assertFalse(equExp1.equals(equExp2));
		assertFalse(equExp2.equals(equExp1));

		equExp2.setPredicate(predicate2);
		assertTrue(equExp1.equals(equExp2));
		assertTrue(equExp2.equals(equExp1));
		assertTrue(equExp1.hashCode() == equExp2.hashCode());
	}

	/**
	 * Il test deve verificare che applicando un valore all'espressione il
	 * predicato venga modificato
	 */
	public void testEvaluate() {
		assertEquals(FuzzyBoolean.UNKNOWN, equExp1.evaluate(ctx));

		predicate1.setValue(FuzzyBoolean.TRUE);
		equExp1.reset();
		assertEquals(FuzzyBoolean.TRUE, equExp1.evaluate(ctx));

		predicate1.setValue(FuzzyBoolean.FALSE);
		equExp1.reset();
		assertEquals(FuzzyBoolean.FALSE, equExp1.evaluate(ctx));
	}

	public void testPredicateExpression() {
		assertSame(predicate1, equExp1.getPredicate());
	}

	public void testScanFalse() {
		predicate1.setValue(FuzzyBoolean.FALSE);
		equExp1.evaluate(ctx);
		context.startHypotesys(hypotesys);
		equExp1.scan(context);
		assertEquals(Weight.NULL_WEIGHT, context.getWeight(predicate1));
		assertEquals(Weight.NULL_WEIGHT, context.getWeight(predicate2));
	}

	public void testScanTrue() {
		predicate1.setValue(FuzzyBoolean.TRUE);
		equExp1.evaluate(ctx);
		context.startHypotesys(hypotesys);
		equExp1.scan(context);
		assertEquals(Weight.NULL_WEIGHT, context.getWeight(predicate1));
		assertEquals(Weight.NULL_WEIGHT, context.getWeight(predicate2));
	}

	public void testScanUnknown() {
		context.startHypotesys(hypotesys);
		equExp1.scan(context);
		assertEquals(W10, context.getWeight(predicate1));
		assertEquals(Weight.NULL_WEIGHT, context.getWeight(predicate2));
	}

	public void testSetPredicate() {
		equExp1.setPredicate(predicate2);
		assertSame(predicate2, equExp1.getPredicate());
	}

	public void testToString() {
		assertEquals("(A)", equExp1.toString());
	}
}