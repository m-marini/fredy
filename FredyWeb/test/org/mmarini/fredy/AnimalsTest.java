package org.mmarini.fredy;

import java.net.URL;

import org.mmarini.fuzzy.Analisys;
import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.IPredicate;
import org.mmarini.fuzzy.Weight;
import org.mmarini.fuzzy.parse.RulesParser;

import junit.framework.TestCase;

/**
 * @author US00852
 * @version $Id: AnimalsTest.java,v 1.2 2005/02/10 22:32:38 marco Exp $
 */
public class AnimalsTest extends TestCase {
	private static final Weight W412 = new Weight(4, 12);
	private static final Weight W35 = new Weight(3, 5);
	private static final Weight W27 = new Weight(2, 7);
	private static final Weight W28 = new Weight(2, 8);
	private static final Weight W29 = new Weight(2, 9);
	private static final Weight W210 = new Weight(2, 10);
	private static final Weight W211 = new Weight(2, 11);
	private static final Weight W13 = new Weight(1, 3);
	private static final Weight W14 = new Weight(1, 4);
	private static final Weight W16 = new Weight(1, 6);
	private static final String RESOURCE_NAME = "/animals-rules.xml";
	private Analisys analisys;
	private Object[][] weightValues;

	/**
	 * 
	 */
	private void checkPredicate(String name, FuzzyBoolean value) {
		IPredicate predicate;
		predicate = analisys.getPredicate(name);
		assertNotNull(name, predicate);
		assertEquals(name, value, predicate.getValue());
	}

	/**
	 * 
	 */
	private void setPredicate(String name, FuzzyBoolean value) {
		IPredicate predicate = analisys.getPredicate(name);
		assertNotNull(name, predicate);
		predicate.setValue(value);
	}

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		URL url = this.getClass().getResource(RESOURCE_NAME);
		RulesParser parser = RulesParser.getInstance();
		analisys = parser.parse(url.toExternalForm());
		weightValues = new Object[][] { { "fulvo", W28 }, { "macchie", W211 },
				{ "strisce", W29 }, { "zampe", W16 }, { "collo", W210 },
				{ "bianconero", W14 }, { "nuota", W13 }, { "ruminante", W27 },
				{ "zoccoli", W27 }, { "peli", W412 }, { "latte", W412 },
				{ "carne", W28 }, { "artigli", W28 }, { "denti", W28 },
				{ "occhi", W28 }, { "piume", W35 }, { "uova", W35 },
				{ "vola", W35 } };
	}

	public void testCarne() {
		setPredicate("carne", FuzzyBoolean.TRUE);
		analisys.analize();
		checkPredicate("carnivoro", FuzzyBoolean.TRUE);
	}

	public void testLatte() {
		setPredicate("latte", FuzzyBoolean.TRUE);
		analisys.analize();
		checkPredicate("mammifero", FuzzyBoolean.TRUE);
	}

	public void testPeli() {
		setPredicate("peli", FuzzyBoolean.TRUE);
		analisys.analize();
		checkPredicate("mammifero", FuzzyBoolean.TRUE);
	}

	public void testTigre() {
		testLatte();
		testCarne();
		setPredicate("fulvo", FuzzyBoolean.TRUE);
		setPredicate("strisce", FuzzyBoolean.TRUE);
		analisys.analize();
		checkPredicate("tigre", FuzzyBoolean.TRUE);
	}

	public void testWeights() {
		analisys.analize();
		for (int i = 0; i < weightValues.length; ++i) {
			String name = (String) weightValues[i][0];
			Weight exp = (Weight) weightValues[i][1];
			Weight wg = analisys.getPredicate(name).getWeight();
			assertEquals(name, exp, wg);
		}
	}
}