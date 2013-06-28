package org.mmarini.fuzzy.handlers;

/**
 * @author US00852
 * @version $Id: SessionHandlerTest4.java,v 1.1.2.1 2004/12/30 01:06:38 marco
 *          Exp $
 */
public class SessionHandlerTest9 extends SessionHandlerTest8 {

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		names = new String[] { "latte", "peli", "piume", "uova", "ruminante",
				"zoccoli", "artigli", "carne", "fulvo" };
		values = new double[] { 0, 1, 0, 0, 0, 0, 1, 1, 1 };

		postulates = new String[] { "macchie", "strisce",

		"bianconero", "collo", "denti", "nuota", "occhi", "vola", "zampe",

		"artigli", "carne", "fulvo", "peli",

		"latte", "piume", "ruminante", "uova", "zoccoli" };
	}
}