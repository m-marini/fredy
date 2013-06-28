package org.mmarini.fuzzy;

import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.Somewhat;

/**
 * @author US00852
 * @version $Id: SomewhatTest.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class SomewhatTest extends UnaryExpTest {
	private static final String EXP_STRING = "somewhat((A))";

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		values = new FuzzyBoolean[][] {
				{ FuzzyBoolean.FALSE, FuzzyBoolean.FALSE.somewhat() },
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_FALSE.somewhat() },
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN.somewhat() },
				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_TRUE.somewhat() },
				{ FuzzyBoolean.TRUE, FuzzyBoolean.TRUE.somewhat() } };
		evalExpression = equalsExpression1 = new Somewhat(constants[0]);
		equalsExpression2 = new Somewhat(constants[0]);
		acceptExpression = new Somewhat(predicateExps[0]);
		expectedString = EXP_STRING;
		scanExp = acceptExpression;
	}
}