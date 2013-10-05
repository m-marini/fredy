package org.mmarini.fuzzy;

import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.IPredicate;
import org.mmarini.fuzzy.PostulateComparator;
import org.mmarini.fuzzy.Predicate;

import junit.framework.TestCase;

/**
 * @author US00852
 * @version $Id: PostulateComparatorTest.java,v 1.1 2004/11/27 10:35:56 marco
 *          Exp $
 */
public class PostulateComparatorTest extends TestCase {
	FuzzyBoolean values[][];
	int expected[];
	IPredicate predicate1;
	IPredicate predicate2;

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		values = new FuzzyBoolean[][] {
				{ FuzzyBoolean.FALSE, FuzzyBoolean.FALSE }, // < 0
				{ FuzzyBoolean.FALSE, FuzzyBoolean.QUITE_FALSE }, // > 0
				{ FuzzyBoolean.FALSE, FuzzyBoolean.UNKNOWN }, // > 0
				{ FuzzyBoolean.FALSE, FuzzyBoolean.QUITE_TRUE }, // > 0
				{ FuzzyBoolean.FALSE, FuzzyBoolean.TRUE }, // > 0

				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.FALSE }, // < 0
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_FALSE }, // < 0
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.UNKNOWN }, // > 0
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_TRUE }, // > 0
				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.TRUE }, // < 0

				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.FALSE }, // < 0
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.QUITE_FALSE }, // < 0
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.UNKNOWN }, // < 0
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.QUITE_TRUE }, // < 0
				{ FuzzyBoolean.UNKNOWN, FuzzyBoolean.TRUE }, // < 0

				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.FALSE }, // < 0
				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_FALSE }, // < 0
				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.UNKNOWN }, // > 0
				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.QUITE_TRUE }, // < 0
				{ FuzzyBoolean.QUITE_TRUE, FuzzyBoolean.TRUE }, // < 0

				{ FuzzyBoolean.TRUE, FuzzyBoolean.FALSE }, // < 0
				{ FuzzyBoolean.TRUE, FuzzyBoolean.QUITE_FALSE }, // > 0
				{ FuzzyBoolean.TRUE, FuzzyBoolean.UNKNOWN }, // > 0
				{ FuzzyBoolean.TRUE, FuzzyBoolean.QUITE_TRUE }, // > 0
				{ FuzzyBoolean.TRUE, FuzzyBoolean.TRUE }, }; // < 0

		expected = new int[] { -1, 1, 1, 1, 1,

		-1, -1, 1, 1, -1,

		-1, -1, -1, -1, -1,

		-1, -1, 1, -1, -1,

		-1, 1, 1, 1, -1 };
		predicate1 = new Predicate("A");
		predicate2 = new Predicate("B");
	}

	public void testCompare() {
		for (int i = 0; i < values.length; ++i) {
			predicate1.setValue(values[i][0]);
			predicate2.setValue(values[i][1]);
			String testName = "Test " + i;
			int diff = PostulateComparator.getInstance().compare(predicate1,
					predicate2);
			assertEquals(testName + " " + values[i][0] + "<" + values[i][1],
					expected[i] < 0, diff < 0);
			assertEquals(testName + " " + values[i][0] + "==" + values[i][1],
					expected[i] == 0, diff == 0);
			assertEquals(testName + " " + values[i][0] + ">" + values[i][1],
					expected[i] > 0, diff > 0);
		}
	}
}