package org.mmarini.fuzzy;

import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.Known;

/**
 * @author US00852
 * @version $Id: KnownTest.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class KnownTest extends UnaryExpTest {
	static final String EXP_STRING = "known((A))";

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		values = new FuzzyBoolean[][] {
				{ FuzzyBoolean.FALSE, FuzzyBoolean.FALSE.known() },
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_FALSE.known() },
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN.known() },
				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_TRUE.known() },
				{ FuzzyBoolean.TRUE, FuzzyBoolean.TRUE.known() } };
		evalExpression = equalsExpression1 = new Known(constants[0]);
		equalsExpression2 = new Known(constants[0]);
		acceptExpression = new Known(predicateExps[0]);
		expectedString = EXP_STRING;
		scanExp = acceptExpression;
		scanValues = new Object[][][] { { { FuzzyBoolean.FALSE }, { W00 } },
				{ { FuzzyBoolean.QUITE_FALSE }, { W00 } },
				{ { FuzzyBoolean.UNKNOWN }, { W00 } },
				{ { FuzzyBoolean.QUITE_TRUE }, { W00 } },
				{ { FuzzyBoolean.TRUE }, { W00 } } };
	}
}