package org.mmarini.fuzzy.handlers;

/**
 * @author US00852
 * @version $Id: SessionHandlerTest4.java,v 1.1.2.1 2004/12/30 01:06:38 marco
 *          Exp $
 */
public class SessionHandlerTest5 extends SessionHandlerTest4 {

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		names = new String[] { "latte", "peli", "piume", "uova", "ruminante" };
		values = new double[] { 0, 1, 0, 0, 0 };

		postulates = new String[] { "zoccoli", "artigli", "carne", "denti",
				"fulvo", "occhi", "strisce", "macchie", "collo", "zampe",

				"bianconero", "nuota", "vola",

				"peli",

				"latte", "piume", "ruminante", "uova" };
	}
}