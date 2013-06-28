package org.mmarini.fuzzy.handlers;

/**
 * @author US00852
 * @version $Id: SessionHandlerTest1.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public class SessionHandlerTest1 extends SessionHandlerDefaultTest {

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		names = new String[] { "latte" };
		values = new double[] { 0 };
		postulates = new String[] { "peli", "piume", "uova", "vola",
				"ruminante", "zoccoli", "artigli", "carne", "denti", "fulvo",
				"occhi", "strisce", "collo", "macchie", "nuota", "bianconero",
				"zampe", "latte" };
	}
}