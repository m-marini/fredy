package org.mmarini.fuzzy.handlers;

import java.io.IOException;
import java.util.List;

import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;

import org.mmarini.fuzzy.Analisys;
import org.mmarini.fuzzy.IPredicate;
import org.mmarini.fuzzy.handlers.SessionHandler;
import org.xml.sax.SAXException;

import junit.framework.TestCase;

/**
 * @author US00852
 * @version $Id: SessionHandlerDefaultTest.java,v 1.1 2004/12/24 14:50:51 marco
 *          Exp $
 */
public class SessionHandlerDefaultTest extends TestCase {
	static final double EPSILON = 1e-10;
	static final String RESOURCE_NAME = "/animals-rules.xml";

	SessionHandler handler;
	String[] names;
	double[] values;
	String[] hypotesys;
	String[] postulates;
	String[] evidences;

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		handler = new SessionHandler();
		names = new String[] {};
		values = new double[] {};
		hypotesys = new String[] { "albatro", "ghepardo", "giraffa",
				"pinguino", "struzzo", "tigre", "zebra" };
		postulates = new String[] { "latte", "peli", "piume", "uova", "vola",
				"ruminante", "zoccoli", "artigli", "carne", "denti", "fulvo",
				"occhi", "strisce", "collo", "macchie", "nuota", "bianconero",
				"zampe" };
		evidences = new String[] { "albatro", "carnivoro", "ghepardo",
				"giraffa", "mammifero", "pinguino", "struzzo", "tigre",
				"uccello", "ungulato", "zebra" };
	}

	public void testAnalize() throws FactoryConfigurationError,
			ParserConfigurationException, SAXException, IOException {
		this.testSetValue();
		handler.analize();
	}

	public void testGetAnalisys() throws FactoryConfigurationError,
			ParserConfigurationException, SAXException, IOException {
		this.testSetResource();
		assertNotNull(handler.getAnalisys());
	}

	public void testGetEvidence() throws FactoryConfigurationError,
			ParserConfigurationException, SAXException, IOException {
		this.testAnalize();
		List list = handler.getEvidence();
		String[] names = evidences;
		assertEquals(names.length, list.size());
		for (int i = 0; i < names.length; ++i) {
			IPredicate predicate = (IPredicate) list.get(i);
			String testName = predicate + ":" + predicate.getWeight();
			assertEquals(testName, names[i], predicate.getName());
		}
	}

	public void testGetHypotesys() throws FactoryConfigurationError,
			ParserConfigurationException, SAXException, IOException {
		this.testAnalize();
		List list = handler.getHypotesys();
		String[] names = hypotesys;
		assertEquals(names.length, list.size());
		for (int i = 0; i < names.length; ++i) {
			IPredicate predicate = (IPredicate) list.get(i);
			String testName = predicate + ":" + predicate.getWeight();
			assertEquals(testName, names[i], predicate.getName());
		}
	}

	public void testGetPostulate() throws FactoryConfigurationError,
			ParserConfigurationException, SAXException, IOException {
		this.testAnalize();
		List list = handler.getPostulate();
		String[] names = postulates;
		assertEquals(names.length, list.size());
		for (int i = 0; i < names.length; ++i) {
			IPredicate predicate = (IPredicate) list.get(i);
			String testName = predicate + ":" + predicate.getWeight();
			assertEquals(testName, names[i], predicate.getName());
		}
	}

	public void testSetResource() {
		handler.setResource(RESOURCE_NAME);
		assertSame(RESOURCE_NAME, handler.getResource());
	}

	public void testSetValue() throws FactoryConfigurationError,
			ParserConfigurationException, SAXException, IOException {
		this.testSetResource();
		Analisys analisys = handler.getAnalisys();
		for (int i = 0; i < names.length; ++i) {
			handler.setValue(names[i], values[i]);
			assertEquals(analisys.getPredicate(names[i]).getValue().getValue(),
					values[i], EPSILON);
		}
	}
}