package org.mmarini.fuzzy.parse;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertThat;

import java.io.IOException;
import java.net.URL;
import java.util.List;

import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;

import org.junit.experimental.theories.DataPoint;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;
import org.mmarini.fuzzy.Rule;
import org.xml.sax.SAXException;

/**
 * @author US00852
 * @version $Id: RulesParserTest.java,v 1.2 2005/02/10 22:32:38 marco Exp $
 */
@RunWith(Theories.class)
public class RulesParserTest {
	public static final URL RESOURCE_URL = RulesParserTest.class
			.getResource("/org/mmarini/fuzzy/parse/test-rules.xml");
	@DataPoint
	public static final String RESOURCE_NAME = RESOURCE_URL.toExternalForm();

	/**
	 * 
	 * @param rules
	 */
	private void checkForResult(List<Rule> rules) {
		assertThat(rules, hasSize(15));
		assertThat(rules.get(0).toString(),
				containsString("if very(P1) then [] else []"));
		assertThat(rules.get(1).toString(),
				containsString("if somewhat(P1) then [] else []"));
		assertThat(rules.get(2).toString(),
				containsString("if known(P1) then [] else []"));
		assertThat(rules.get(3).toString(),
				containsString("if not(P1) then [] else []"));
		assertThat(rules.get(4).toString(),
				containsString("if ifOnlyIf[P1, P2] then [] else []"));
		assertThat(rules.get(5).toString(),
				containsString("if implies[P1, P2] then [] else []"));
		assertThat(rules.get(6).toString(),
				containsString("if and[P1, P2, P3] then [] else []"));
		assertThat(rules.get(7).toString(),
				containsString("if or[P1, P2, P3] then [] else []"));
		assertThat(
				rules.get(8).toString(),
				containsString("if P1 then [H1=false, H2=true] else [H3=false, E1=true]"));
		assertThat(rules.get(9).toString(),
				containsString("if false(0.0) then [] else []"));
		assertThat(rules.get(10).toString(),
				containsString("if quite.false(0.25) then [] else []"));
		assertThat(rules.get(11).toString(),
				containsString("if unknown(0.5) then [] else []"));
		assertThat(rules.get(12).toString(),
				containsString("if quite.true(0.75) then [] else []"));
		assertThat(rules.get(13).toString(),
				containsString("if true(1.0) then [] else []"));
		assertThat(rules.get(14).toString(),
				containsString("if quite.true(0.8) then [] else []"));
	}

	public void testGetInstance() {
		assertThat(RulesParser.getInstance(), notNullValue());
	}

	/*
	 * Class under test for Analisys parse(String)
	 */
	@Theory
	public void testParseString(String resource) throws SAXException,
			FactoryConfigurationError, ParserConfigurationException,
			IOException {
		RulesParser parser = new RulesParser();
		parser.setValidating(true);
		List<Rule> rules = parser.parse(resource);
		assertThat(rules, notNullValue());
		checkForResult(rules);
	}
}