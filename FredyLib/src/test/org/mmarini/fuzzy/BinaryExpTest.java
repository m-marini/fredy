package org.mmarini.fuzzy;

import org.mmarini.fuzzy.Constant;
import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.Predicate;
import org.mmarini.fuzzy.PredicateExpression;

/**
 * @author US00852
 * @version $Id: BinaryExpTest.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class BinaryExpTest extends UnaryExpTest {

	/**
	 * @see junit.framework.TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		constants = new Constant[] { new Constant(), new Constant() };
		predicates = new Predicate[] { new Predicate("A"), new Predicate("B") };
		predicateExps = new PredicateExpression[] {
				new PredicateExpression(predicates[0]),
				new PredicateExpression(predicates[1]) };
		initValues = new FuzzyBoolean[][] { { FuzzyBoolean.UNKNOWN,
				FuzzyBoolean.UNKNOWN } };
	}

	@Override
	public void testEvaluate() {
		for (int i = 0; i < values.length; ++i) {
			constants[0].setValue(values[i][0]);
			constants[1].setValue(values[i][1]);
			evalExpression.reset();
			assertEquals(values[i][0] + "," + values[i][1], values[i][2],
					equalsExpression1.evaluate(null));
			constants[1].setValue(values[i][1]);
			constants[0].setValue(values[i][0]);
			evalExpression.reset();
			assertEquals(values[i][1] + "," + values[i][0], values[i][2],
					equalsExpression1.evaluate(null));
		}
	}
}