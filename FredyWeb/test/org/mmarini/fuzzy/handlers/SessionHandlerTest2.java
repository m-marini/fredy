package org.mmarini.fuzzy.handlers;

/**
 * @author US00852
 * @version $Id: SessionHandlerTest2.java,v 1.1.2.1 2004/12/30 01:06:38 marco
 *          Exp $
 */
public class SessionHandlerTest2 extends SessionHandlerDefaultTest {

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		names = new String[] { "latte", "peli" };
		values = new double[] { 0, 1 };
		postulates = new String[] { "piume", "uova", "vola", "ruminante",
				"zoccoli", "artigli", "carne", "denti", "fulvo", "occhi",
				"strisce", "collo", "macchie", "nuota", "bianconero", "zampe",

				"peli", "latte" };
		evidences = new String[] { "mammifero", "albatro", "carnivoro",
				"ghepardo", "giraffa", "pinguino", "struzzo", "tigre",
				"uccello", "ungulato", "zebra" };
	}
}