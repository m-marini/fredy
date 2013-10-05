package org.mmarini.fuzzy;

import org.mmarini.fuzzy.Assign;
import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.IAssignVisitor;
import org.mmarini.fuzzy.IPredicate;
import org.mmarini.fuzzy.Predicate;

import junit.framework.TestCase;

/**
 * @author US00852
 * @version $Id: AssignTest.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class AssignTest extends TestCase {
	static final String EXP_STRING = "(A)";

	Assign exp1;
	Assign exp2;
	Assign calcExp;
	IPredicate predicateA;
	IPredicate predicateB;
	FuzzyBoolean values[][];
	boolean visitorCalled;

	/**
	 * @see junit.framework.TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		values = new FuzzyBoolean[][] {
				{ FuzzyBoolean.FALSE, FuzzyBoolean.FALSE, FuzzyBoolean.FALSE },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.QUITE_FALSE,
						FuzzyBoolean.QUITE_FALSE },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.UNKNOWN,
						FuzzyBoolean.UNKNOWN },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.QUITE_TRUE,
						FuzzyBoolean.QUITE_TRUE },
				{ FuzzyBoolean.FALSE, FuzzyBoolean.TRUE, FuzzyBoolean.TRUE },

				{ FuzzyBoolean.QUITE_FALSE, FuzzyBoolean.QUITE_FALSE,
						FuzzyBoolean.QUITE_FALSE },
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

				{ FuzzyBoolean.TRUE, FuzzyBoolean.TRUE, FuzzyBoolean.TRUE } };

		predicateA = new Predicate("A");
		predicateB = new Predicate("B");
		exp1 = new Assign(predicateA);
		exp2 = new Assign(predicateA);
		calcExp = new Assign(predicateA);
	}

	public void testAccept() {
		exp1.accept(new IAssignVisitor() {

			@Override
			public void visitPredicate(IPredicate predicate) {
				assertEquals(AssignTest.this.predicateA, predicate);
				visitorCalled = true;
			}
		});
		assertTrue(visitorCalled);
	}

	public void testAssign() {
		for (int i = 0; i < values.length; ++i) {
			predicateA.setValue(values[i][0]);
			exp1.assign(values[i][1]);
			assertEquals(values[i][0] + "," + values[i][1], values[i][2],
					predicateA.getValue());

			predicateA.setValue(values[i][1]);
			exp1.assign(values[i][0]);
			assertEquals(values[i][1] + "," + values[i][0], values[i][2],
					predicateA.getValue());
		}
	}

	public void testEquals() {
		assertTrue(exp1.equals(exp1));
		assertFalse(exp1.equals(null));
		assertFalse(exp1.equals(new Object()));

		assertTrue(exp1.equals(exp2));
		assertTrue(exp2.equals(exp1));
		assertTrue(exp1.hashCode() == exp2.hashCode());
	}

	public void testIsAssigner() {
		assertTrue(calcExp.isAssigner(predicateA));
		assertFalse(calcExp.isAssigner(predicateB));
	}

	public void testToString() {
		assertEquals(EXP_STRING, exp1.toString());
	}
}