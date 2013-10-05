package org.mmarini.fuzzy.handlers;

/**
 * @author US00852
 * @version $Id: SessionHandlerTest3.java,v 1.1.2.1 2004/12/30 01:06:38 marco
 *          Exp $
 */
public class SessionHandlerTest3 extends SessionHandlerDefaultTest {

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		names = new String[] { "latte", "peli", "piume" };
		values = new double[] { 0, 1, 0 };

		postulates = new String[] { "uova", "vola", "ruminante", "zoccoli",
				"artigli", "carne", "denti", "fulvo", "occhi", "collo",
				"strisce", "macchie", "nuota", "bianconero", "zampe",

				"peli", "latte", "piume" };

		evidences = new String[] { "mammifero", "albatro", "carnivoro",
				"ghepardo", "giraffa", "pinguino", "struzzo", "tigre",
				"uccello", "ungulato", "zebra" };

	}
}