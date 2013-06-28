package org.mmarini.fuzzy;

import org.mmarini.fuzzy.And;
import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.Weight;

/**
 * @author US00852
 * @version $Id: AndTest.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class AndTest extends NupleExpTest {
	private static final String EXP_STRING = "and(unknown(0.5),unknown(0.5),unknown(0.5))";
	private static final Weight W11 = new Weight(1, 1);

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		values = new FuzzyBoolean[][] {
				{ FuzzyBoolean.FALSE, FuzzyBoolean.FALSE, FuzzyBoolean.FALSE },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.QUITE_FALSE,
						FuzzyBoolean.FALSE },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.UNKNOWN, FuzzyBoolean.FALSE },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.FALSE },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.TRUE, FuzzyBoolean.FALSE },

				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_FALSE,
						FuzzyBoolean.QUITE_FALSE },
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.UNKNOWN,
						FuzzyBoolean.QUITE_FALSE },
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.QUITE_FALSE },
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.TRUE,
						FuzzyBoolean.QUITE_FALSE },

				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN,
						FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.TRUE, FuzzyBoolean.UNKNOWN },

				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.QUITE_TRUE },
				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.TRUE,
						FuzzyBoolean.QUITE_TRUE },
				{ FuzzyBoolean.TRUE, FuzzyBoolean.TRUE, FuzzyBoolean.TRUE }, };

		values3 = new FuzzyBoolean[][] {
				{ FuzzyBoolean.FALSE, FuzzyBoolean.FALSE, FuzzyBoolean.FALSE,
						FuzzyBoolean.FALSE },

				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_FALSE,
						FuzzyBoolean.FALSE, FuzzyBoolean.FALSE },
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_FALSE,
						FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_FALSE },

				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN,
						FuzzyBoolean.FALSE, FuzzyBoolean.FALSE },
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN,
						FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_FALSE },
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN,
						FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN },

				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.FALSE, FuzzyBoolean.FALSE },
				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_FALSE },
				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_TRUE },

				{ FuzzyBoolean.TRUE, FuzzyBoolean.TRUE, FuzzyBoolean.FALSE,
						FuzzyBoolean.FALSE },
				{ FuzzyBoolean.TRUE, FuzzyBoolean.TRUE,
						FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_FALSE },
				{ FuzzyBoolean.TRUE, FuzzyBoolean.TRUE, FuzzyBoolean.UNKNOWN,
						FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.TRUE, FuzzyBoolean.TRUE,
						FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_TRUE },
				{ FuzzyBoolean.TRUE, FuzzyBoolean.TRUE, FuzzyBoolean.TRUE,
						FuzzyBoolean.TRUE }, };
		evalExpression = equalsExpression1 = new And(constants[0], constants[1]);
		equalsExpression2 = new And(constants[0], constants[1]);
		acceptExpression = new And(predicateExps[0], predicateExps[1]);
		expectedString = EXP_STRING;

		evalExpression3 = new And(constants[0], constants[1]);
		evalExpression3.addParameter(constants[2]);

		scanExp = acceptExpression;
		initValues = new FuzzyBoolean[][] {
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.FALSE },
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.TRUE },
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.TRUE, FuzzyBoolean.UNKNOWN }, };

		scanValues = new Object[][][] {
				{ { FuzzyBoolean.FALSE, FuzzyBoolean.FALSE }, { W00, W00 } },
				{ { FuzzyBoolean.FALSE, FuzzyBoolean.UNKNOWN }, { W00, W00 } },
				{ { FuzzyBoolean.FALSE, FuzzyBoolean.TRUE }, { W00, W00 } },
				{ { FuzzyBoolean.UNKNOWN, FuzzyBoolean.FALSE }, { W00, W00 } },
				{ { FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN }, { W11, W11 } },
				{ { FuzzyBoolean.UNKNOWN, FuzzyBoolean.TRUE }, { W10, W00 } },
				{ { FuzzyBoolean.TRUE, FuzzyBoolean.FALSE }, { W00, W00 } },
				{ { FuzzyBoolean.TRUE, FuzzyBoolean.UNKNOWN }, { W00, W10 } },
				{ { FuzzyBoolean.TRUE, FuzzyBoolean.TRUE }, { W00, W00 } } };

	}
}