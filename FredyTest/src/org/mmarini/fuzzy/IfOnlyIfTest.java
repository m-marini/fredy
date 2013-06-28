package org.mmarini.fuzzy;

import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.IfOnlyIf;

/**
 * @author US00852
 * @version $Id: IfOnlyIfTest.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class IfOnlyIfTest extends BinaryExpTest {
	static final String EXP_STRING = "ifonlyif((A),(B))";

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		values = new FuzzyBoolean[][] {
				{ FuzzyBoolean.FALSE, FuzzyBoolean.FALSE, FuzzyBoolean.TRUE },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.QUITE_FALSE,
						FuzzyBoolean.QUITE_TRUE },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.UNKNOWN,
						FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.QUITE_FALSE },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.TRUE, FuzzyBoolean.FALSE },

				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_FALSE,
						FuzzyBoolean.TRUE },
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.UNKNOWN,
						FuzzyBoolean.QUITE_TRUE },
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.TRUE,
						FuzzyBoolean.QUITE_FALSE },

				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN, FuzzyBoolean.TRUE },
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.QUITE_TRUE },
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.TRUE, FuzzyBoolean.UNKNOWN },

				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.TRUE },
				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.TRUE,
						FuzzyBoolean.QUITE_TRUE },
				{ FuzzyBoolean.TRUE, FuzzyBoolean.TRUE, FuzzyBoolean.TRUE }, };
		evalExpression = equalsExpression1 = new IfOnlyIf(constants[0],
				constants[1]);
		equalsExpression2 = new IfOnlyIf(constants[0], constants[1]);
		acceptExpression = new IfOnlyIf(predicateExps[0], predicateExps[1]);
		expectedString = EXP_STRING;

		scanExp = acceptExpression;

		initValues = new FuzzyBoolean[][] {
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.FALSE },
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.TRUE },
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.TRUE, FuzzyBoolean.UNKNOWN }, };

		scanValues = new Object[][][] {
				{ { FuzzyBoolean.FALSE, FuzzyBoolean.FALSE }, { W00, W00 } },
				{ { FuzzyBoolean.FALSE, FuzzyBoolean.UNKNOWN }, { W00, W10 } },
				{ { FuzzyBoolean.FALSE, FuzzyBoolean.TRUE }, { W00, W00 } },
				{ { FuzzyBoolean.UNKNOWN, FuzzyBoolean.FALSE }, { W10, W00 } },
				{ { FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN }, { W00, W00 } },
				{ { FuzzyBoolean.UNKNOWN, FuzzyBoolean.TRUE }, { W10, W00 } },
				{ { FuzzyBoolean.TRUE, FuzzyBoolean.FALSE }, { W00, W00 } },
				{ { FuzzyBoolean.TRUE, FuzzyBoolean.UNKNOWN }, { W00, W10 } },
				{ { FuzzyBoolean.TRUE, FuzzyBoolean.TRUE }, { W00, W00 } } };
	}
}