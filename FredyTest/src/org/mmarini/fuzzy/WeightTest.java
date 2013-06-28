package org.mmarini.fuzzy;

import org.mmarini.fuzzy.Weight;

import junit.framework.TestCase;

/**
 * @author US00852
 * @version $Id: WeightTest.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class WeightTest extends TestCase {
	static final String WEIGHT12_STRING = "(w=1,d=2)";
	static final String WEIGHT0_STRING = "(w=0,d=0)";

	static final int DEPENDENCE_COUNT_VALUE = 2;
	static final int WEIGHT_VALUE = 1;
	static final int HV = 20;
	static final int LV = 10;

	static final Weight W0 = new Weight();
	static final Weight W0H = new Weight(0, HV);
	static final Weight WHH = new Weight(HV, HV);
	static final Weight W0L = new Weight(0, LV);
	static final Weight WLH = new Weight(LV, HV);
	static final Weight WLL = new Weight(LV, LV);
	static final Weight WHL = new Weight(HV, LV);

	Weight equals1;
	Weight equals2;
	Weight weight12;
	Weight weight0;
	Weight[] compareList;
	Weight[][] addList;

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		weight0 = new Weight();
		weight12 = new Weight(WEIGHT_VALUE, DEPENDENCE_COUNT_VALUE);
		equals1 = new Weight();
		equals2 = new Weight();
		compareList = new Weight[] { WHL, WHH, WLL, WLH, W0, W0L, W0H };
		addList = new Weight[][] {
				{ new Weight(0, 0), new Weight(0, 0), new Weight(0, 0) },
				{ new Weight(0, 1), new Weight(0, 2), new Weight(0, 3) },
				{ new Weight(1, 0), new Weight(2, 0), new Weight(3, 0) },
				{ new Weight(1, 1), new Weight(2, 2), new Weight(3, 3) } };
	}

	public void testAdd() {
		for (int i = 0; i < addList.length; ++i) {
			Weight[] values = addList[i];
			assertEquals(values[0] + "+" + values[1], values[2],
					values[0].add(values[1]));
			assertEquals(values[1] + "+" + values[0], values[2],
					values[1].add(values[0]));
		}
	}

	public void testCompareTo() {
		for (int i = 0; i < compareList.length; ++i) {
			Weight compare1 = compareList[i];
			assertTrue(compare1.toString(), compare1.compareTo(compare1) == 0);
			for (int j = i + 1; j < compareList.length; ++j) {
				Weight compare2 = compareList[j];
				assertTrue(compare1 + ">" + compare2,
						compare1.compareTo(compare2) > 0);
				assertTrue(compare2 + "<" + compare1,
						compare2.compareTo(compare1) < 0);
			}
		}
	}

	/*
	 * Class under test for boolean equals(Object)
	 */
	public void testEqualsObject() {
		assertTrue(equals1.equals(equals1));
		assertFalse(equals1.equals(null));
		assertFalse(equals1.equals(new Object()));

		assertTrue(equals1.equals(equals2));
		assertTrue(equals2.equals(equals1));
		assertTrue(equals1.hashCode() == equals2.hashCode());

		equals1.setWeight(WEIGHT_VALUE);
		assertFalse(equals1.equals(equals2));
		assertFalse(equals2.equals(equals1));

		equals2.setWeight(WEIGHT_VALUE);
		assertTrue(equals1.equals(equals2));
		assertTrue(equals2.equals(equals1));
		assertTrue(equals1.hashCode() == equals2.hashCode());

		equals1.setDependeceCount(DEPENDENCE_COUNT_VALUE);
		assertFalse(equals1.equals(equals2));
		assertFalse(equals2.equals(equals1));

		equals2.setDependeceCount(DEPENDENCE_COUNT_VALUE);
		assertTrue(equals1.equals(equals2));
		assertTrue(equals2.equals(equals1));
		assertTrue(equals1.hashCode() == equals2.hashCode());
	}

	public void testMax() {
		assertEquals(compareList[0], compareList[0].max(compareList[1]));
		assertEquals(compareList[0], compareList[1].max(compareList[0]));
	}

	public void testMin() {
		assertEquals(compareList[1], compareList[0].min(compareList[1]));
		assertEquals(compareList[1], compareList[1].min(compareList[0]));
	}

	public void testSetDependeceCount() {
		weight0.setDependeceCount(DEPENDENCE_COUNT_VALUE);
		assertEquals(DEPENDENCE_COUNT_VALUE, weight0.getDependeceCount());
	}

	public void testSetWeight() {
		weight0.setWeight(WEIGHT_VALUE);
		assertEquals(WEIGHT_VALUE, weight0.getWeight());
	}

	/*
	 * Class under test for String toString()
	 */
	public void testToString() {
		assertEquals(WEIGHT0_STRING, weight0.toString());
		assertEquals(WEIGHT12_STRING, weight12.toString());
	}

	/*
	 * Class under test for void Weight()
	 */
	public void testWeight() {
		assertEquals(0, weight0.getWeight());
		assertEquals(0, weight0.getDependeceCount());
	}

	/*
	 * Class under test for void Weight(int, int)
	 */
	public void testWeightintint() {
		assertEquals(WEIGHT_VALUE, weight12.getWeight());
		assertEquals(DEPENDENCE_COUNT_VALUE, weight12.getDependeceCount());
	}
}