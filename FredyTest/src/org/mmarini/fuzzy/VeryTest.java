package org.mmarini.fuzzy;

import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.Very;

/**
 * @author US00852
 * @version $Id: VeryTest.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class VeryTest extends UnaryExpTest {
	private static final String EXP_STRING = "very((A))";

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		values = new FuzzyBoolean[][] {
				{ FuzzyBoolean.FALSE, FuzzyBoolean.FALSE.very() },
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_FALSE.very() },
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN.very() },
				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_TRUE.very() },
				{ FuzzyBoolean.TRUE, FuzzyBoolean.TRUE.very() } };
		evalExpression = equalsExpression1 = new Very(constants[0]);
		equalsExpression2 = new Very(constants[0]);
		acceptExpression = new Very(predicateExps[0]);
		expectedString = EXP_STRING;
		scanExp = acceptExpression;
	}
}