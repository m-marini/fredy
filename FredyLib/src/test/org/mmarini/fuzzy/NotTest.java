package org.mmarini.fuzzy;

import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.Not;

/**
 * @author US00852
 * @version $Id: NotTest.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class NotTest extends UnaryExpTest {
	static final String EXP_STRING = "not((A))";

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		values = new FuzzyBoolean[][] {
				{ FuzzyBoolean.FALSE, FuzzyBoolean.FALSE.not() },
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_FALSE.not() },
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN.not() },
				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_TRUE.not() },
				{ FuzzyBoolean.TRUE, FuzzyBoolean.TRUE.not() } };
		evalExpression = equalsExpression1 = new Not(constants[0]);
		equalsExpression2 = new Not(constants[0]);
		acceptExpression = new Not(predicateExps[0]);
		expectedString = EXP_STRING;

		scanExp = acceptExpression;
	}
}