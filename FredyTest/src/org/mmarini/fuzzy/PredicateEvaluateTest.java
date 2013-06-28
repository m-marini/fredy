package org.mmarini.fuzzy;

import org.mmarini.fuzzy.Analisys;
import org.mmarini.fuzzy.Assign;
import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.IPredicate;
import org.mmarini.fuzzy.PredicateExpression;
import org.mmarini.fuzzy.Rule;

import junit.framework.TestCase;

/**
 * @author US00852
 * @version $Id: PredicateEvaluateTest.java,v 1.2 2005/02/10 22:32:35 marco Exp
 *          $
 */
public class PredicateEvaluateTest extends TestCase {
	Analisys analisys;
	IPredicate ifPredicate1;
	IPredicate ifPredicate2;
	PredicateExpression ifExp1;
	PredicateExpression ifExp2;
	IPredicate thenPredicate;
	Rule rule1;
	Rule rule2;
	FuzzyBoolean[][] values;

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		values = new FuzzyBoolean[][] {
				{ FuzzyBoolean.FALSE, FuzzyBoolean.FALSE, FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.QUITE_FALSE,
						FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.UNKNOWN,
						FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.QUITE_TRUE },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.TRUE, FuzzyBoolean.TRUE },

				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_FALSE,
						FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.UNKNOWN,
						FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.QUITE_TRUE },
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.TRUE,
						FuzzyBoolean.TRUE },

				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN,
						FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.QUITE_TRUE },
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.TRUE, FuzzyBoolean.TRUE },

				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.QUITE_TRUE },
				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.TRUE, FuzzyBoolean.TRUE },

				{ FuzzyBoolean.TRUE, FuzzyBoolean.TRUE, FuzzyBoolean.TRUE }, };

		analisys = new Analisys();

		ifPredicate1 = analisys.addPredicate("ifPredicate1");
		ifPredicate2 = analisys.addPredicate("ifPredicate2");
		thenPredicate = analisys.addPredicate("thenPredicate");
		ifExp1 = new PredicateExpression(ifPredicate1);
		ifExp2 = new PredicateExpression(ifPredicate2);
		rule1 = new Rule();
		rule1.setIfExpression(ifExp1);
		rule1.addThenExpression(new Assign(thenPredicate));
		analisys.addRule(rule1);

		rule2 = new Rule();
		rule2.setIfExpression(ifExp2);
		rule2.addThenExpression(new Assign(thenPredicate));
		analisys.addRule(rule2);
	}

	public void testEvaluate() {
		for (int i = 0; i < values.length; ++i) {
			rule1.reset();
			rule2.reset();
			ifPredicate1.setValue(values[i][0]);
			ifPredicate2.setValue(values[i][1]);
			thenPredicate.setValue(FuzzyBoolean.UNKNOWN);
			FuzzyBoolean result = analisys.evaluate(thenPredicate);
			assertEquals(values[i][0] + "," + values[i][1], values[i][2],
					result);

			rule1.reset();
			rule2.reset();
			ifPredicate1.setValue(values[i][1]);
			ifPredicate2.setValue(values[i][0]);
			thenPredicate.setValue(FuzzyBoolean.UNKNOWN);
			result = analisys.evaluate(thenPredicate);
			assertEquals(values[i][1] + "," + values[i][0], values[i][2],
					result);
		}
	}
}