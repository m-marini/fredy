/*
 * Created on 13-nov-2004
 */
package org.mmarini.fuzzy;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.hamcrest.CoreMatchers.*;
import org.mmarini.fuzzy.Constant;
import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.Predicate;
import org.mmarini.fuzzy.PredicateExpression;
import org.mmarini.fuzzy.Weight;

/**
 * @author US00852
 * @version $Id: UnaryExpTest.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class UnaryExpTest extends AbstractExpressionTest {
	protected static final Weight W00 = Weight.NULL_WEIGHT;
	protected static final Weight W10 = new Weight(1, 0);
	Constant[] constants;
	FuzzyBoolean[][] values;
	FuzzyBoolean[][] initValues;

	/**
	 * @see junit.framework.TestCase#setUp()
	 */
	@Override
	public void init() throws Exception {
		super.init();
		constants = new Constant[] { new Constant() };
		predicates = new Predicate[] { new Predicate("A") };
		predicateExps = new PredicateExpression[] { new PredicateExpression(
				predicates[0]) };
		initValues = new FuzzyBoolean[][] { { FuzzyBoolean.UNKNOWN } };
		scanValues = new Object[][][] { { { FuzzyBoolean.FALSE }, { W00 } },
				{ { FuzzyBoolean.UNKNOWN }, { W10 } },
				{ { FuzzyBoolean.TRUE }, { W00 } } };
	}

	@Test
	public void testEvaluateFalse() {
		constants[0].setValue(FuzzyBoolean.FALSE);
		evalExpression.reset();
		assertThat(evalExpression.evaluate(null), is(equalTo(W00)));
	}

	@Test
	public void testEvaluate() {
		for (int i = 0; i < values.length; ++i) {
			constants[0].setValue(values[i][0]);
			evalExpression.reset();
			assertEquals(values[i][0].toString(), values[i][1],
					evalExpression.evaluate(null));
		}
	}
}