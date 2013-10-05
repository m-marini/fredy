package org.mmarini.fuzzy.parse;

import java.net.URL;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import junit.framework.TestCase;

import org.mmarini.fuzzy.Analisys;
import org.mmarini.fuzzy.Predicate;
import org.mmarini.fuzzy.parse.RulesParser;
import org.xml.sax.InputSource;
import org.xml.sax.SAXParseException;

/**
 * @author US00852
 * @version $Id: RulesParserTest.java,v 1.2 2005/02/10 22:32:38 marco Exp $
 */
public class RulesParserTest extends TestCase {
	public static final String RESOURCE_NAME = "/org/mmarini/fuzzy/parse/test-rules.xml";

	RulesParser parser;
	URL url;
	Set ruleStrings;

	/**
	 * 
	 */
	private void checkForResult(Analisys analisys) {
		assertNotNull(analisys);
		List list = analisys.getHypotesys();
		assertEquals(4, list.size());
		for (int i = 0; i < 4; ++i) {
			String name = "H" + (i + 1);
			assertTrue(name, list.contains(new Predicate(name)));
		}
		list = analisys.getPostulate();
		assertEquals(3, list.size());
		for (int i = 0; i < 3; ++i) {
			String name = "P" + (i + 1);
			assertTrue(name, list.contains(new Predicate(name)));
		}
		this.checkForRules(analisys);
	}

	/**
	 * @param analisys
	 */
	private void checkForRules(Analisys analisys) {
		List rules = analisys.getRule();
		assertEquals(rules.size(), ruleStrings.size());
		for (Iterator iter = rules.iterator(); iter.hasNext();) {
			String rule = iter.next().toString();
			assertTrue(rule, ruleStrings.contains(rule));
		}
	}

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		url = this.getClass().getResource(RESOURCE_NAME);
		parser = RulesParser.getInstance();
		parser.setValidating(true);
		ruleStrings = new HashSet();
		// ruleStrings
		// .add("if(and((P1),or((P2),ifonlyif((P3),(P4))))then(not(H1),(H2))else(not(H3),(E1))");
		ruleStrings.add("if(very((P1)))then()else()");
		ruleStrings.add("if(somewhat((P1)))then()else()");
		ruleStrings.add("if(not((P1)))then()else()");
		ruleStrings.add("if(known((P1)))then()else()");
		ruleStrings.add("if(ifonlyif((P1),(P2)))then()else()");
		ruleStrings.add("if(implies((P1),(P2)))then()else()");
		ruleStrings.add("if(and((P1),(P2),(P3)))then()else()");
		ruleStrings.add("if(or((P1),(P2),(P3)))then()else()");
		ruleStrings.add("if((P1))then(not(H1),(H2))else(not(H3),(E1))");
		ruleStrings.add("if(not((E1)))then((H4))else()");
	}

	public void testGetInstance() {
		assertNotNull(parser);
	}

	/*
	 * Class under test for Analisys parse(InputSource)
	 */
	public void testParseInputSource() {
		assertNotNull(url);
		String uri = url.toExternalForm();
		assertNotNull(uri);
		InputSource source = new InputSource(uri);
		Analisys analisys = null;
		try {
			analisys = parser.parse(source);
		} catch (SAXParseException e) {
			String message = e.getSystemId() + "(" + e.getLineNumber() + ", "
					+ e.getColumnNumber() + "): " + e.getMessage();
			fail(message);
		} catch (Exception e) {
			e.printStackTrace();
			fail(e.getMessage());
		}
		assertNotNull(analisys);
		this.checkForResult(analisys);
	}

	/*
	 * Class under test for Analisys parse(String)
	 */
	public void testParseString() {
		assertNotNull(url);
		String uri = url.toExternalForm();
		assertNotNull(uri);
		Analisys analisys = null;
		try {
			analisys = parser.parse(uri);
		} catch (SAXParseException e) {
			String message = e.getSystemId() + "(" + e.getLineNumber() + ", "
					+ e.getColumnNumber() + "): " + e.getMessage();
			fail(message);
		} catch (Exception e) {
			e.printStackTrace();
			fail(e.getMessage());
		}
		assertNotNull(analisys);
		this.checkForResult(analisys);
	}
}