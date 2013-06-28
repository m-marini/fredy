package org.mmarini.fuzzy;

import org.mmarini.fuzzy.AbstractExpression;
import org.mmarini.fuzzy.Constant;
import org.mmarini.fuzzy.FuzzyBoolean;

/**
 * @author US00852
 * @version $Id: NupleExpTest.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class NupleExpTest extends BinaryExpTest {
	FuzzyBoolean[][] values3;
	AbstractExpression evalExpression3;

	/**
	 * @see junit.framework.TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		constants = new Constant[] { new Constant(), new Constant(),
				new Constant() };
	}

	@Override
	public void testEvaluate() {
		for (int i = 0; i < values.length; ++i) {
			constants[0].setValue(values[i][0]);
			constants[1].setValue(values[i][1]);
			evalExpression.reset();
			assertEquals(values[i][0] + "," + values[i][1], values[i][2],
					evalExpression.evaluate(null));
			constants[1].setValue(values[i][1]);
			constants[0].setValue(values[i][0]);
			evalExpression.reset();
			assertEquals(values[i][1] + "," + values[i][0], values[i][2],
					evalExpression.evaluate(null));
		}
	}

	public void testEvaluate3() {
		for (int i = 0; i < values3.length; ++i) {
			assertEquals(4, values3[i].length);
			constants[0].setValue(values3[i][0]);
			constants[1].setValue(values3[i][1]);
			constants[2].setValue(values3[i][2]);
			evalExpression3.reset();
			assertEquals(values3[i][0] + "," + values3[i][1] + ","
					+ values3[i][2], values3[i][3],
					evalExpression3.evaluate(null));
		}
	}

	@Override
	public void testToString() {
		assertEquals(expectedString, evalExpression3.toString());
	}
}