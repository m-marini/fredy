package org.mmarini.fuzzy.handlers;

/**
 * @author US00852
 * @version $Id: SessionHandlerTest4.java,v 1.1.2.1 2004/12/30 01:06:38 marco
 *          Exp $
 */
public class SessionHandlerTest4 extends SessionHandlerDefaultTest {

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		names = new String[] { "latte", "peli", "piume", "uova" };
		values = new double[] { 0, 1, 0, 0 };

		postulates = new String[] { "ruminante", "zoccoli", "artigli", "carne",
				"denti", "fulvo", "occhi", "strisce", "macchie", "collo",
				"zampe",

				"bianconero", "nuota", "vola",

				"peli", "latte", "piume", "uova" };

		evidences = new String[] { "mammifero", "albatro", "pinguino",
				"struzzo", "uccello", "carnivoro", "ghepardo", "giraffa",
				"tigre", "ungulato", "zebra" };

		hypotesys = new String[] { "albatro", "pinguino", "struzzo",
				"ghepardo", "giraffa", "tigre", "zebra" };
	}
}