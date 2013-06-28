package org.mmarini.fuzzy.handlers;

/**
 * @author US00852
 * @version $Id: SessionHandlerTest4.java,v 1.1.2.1 2004/12/30 01:06:38 marco
 *          Exp $
 */
public class SessionHandlerTest6 extends SessionHandlerTest4 {

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		names = new String[] { "latte", "peli", "piume", "uova", "ruminante",
				"zoccoli" };
		values = new double[] { 0, 1, 0, 0, 0, 0 };

		postulates = new String[] { "artigli", "carne", "denti", "fulvo",
				"occhi", "macchie", "strisce",

				"bianconero", "collo", "nuota", "vola", "zampe",

				"peli",

				"latte", "piume", "ruminante", "uova", "zoccoli" };

		evidences = new String[] { "mammifero", "albatro", "giraffa",
				"pinguino", "struzzo", "uccello", "ungulato", "zebra",

				"carnivoro", "ghepardo", "tigre" };

		hypotesys = new String[] { "albatro", "giraffa", "pinguino", "struzzo",
				"zebra", "ghepardo", "tigre" };
	}
}