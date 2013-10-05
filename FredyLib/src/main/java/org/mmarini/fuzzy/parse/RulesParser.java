package org.mmarini.fuzzy.parse;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URL;
import java.util.List;

import javax.xml.XMLConstants;
import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.mmarini.fuzzy.Rule;
import org.xml.sax.SAXException;

/**
 * @author US00852
 * @version $Id: RulesParser.java,v 1.2 2005/02/10 22:32:38 marco Exp $
 */
public class RulesParser {
	public static final String RULES_SCHEMA_RESOURCE = "/rules.xsd";
	private static RulesParser instance = new RulesParser();

	/**
	 * Returns the singleton
	 * 
	 * @return the instance.
	 */
	public static RulesParser getInstance() {
		return instance;
	}

	private Schema schemaValidator;

	/**
	 * 
	 */
	protected RulesParser() {
	}

	/**
	 * Creates the parser
	 * 
	 * @return the parser
	 * @throws FactoryConfigurationError
	 *             in case of error
	 * @throws ParserConfigurationException
	 *             in case of error
	 * @throws SAXException
	 *             in case of error
	 */
	private SAXParser createParser() throws FactoryConfigurationError,
			ParserConfigurationException, SAXException {
		SAXParserFactory factory = SAXParserFactory.newInstance();
		if (schemaValidator != null) {
			factory.setSchema(schemaValidator);
			factory.setNamespaceAware(true);
		}
		SAXParser parser = factory.newSAXParser();
		return parser;
	}

	/**
	 * @throws SAXException
	 * @throws FileNotFoundException
	 */
	private Schema createSchema() throws SAXException, FileNotFoundException {
		URL schemaUrl = this.getClass().getResource(RULES_SCHEMA_RESOURCE);
		if (schemaUrl == null)
			throw new FileNotFoundException(RULES_SCHEMA_RESOURCE);
		Schema schema;
		SchemaFactory factory = SchemaFactory
				.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
		schema = factory.newSchema(schemaUrl);
		return schema;
	}

	/**
	 * Parses the uri
	 * 
	 * @param uri
	 *            the uri
	 * @return the rule list
	 * @throws FactoryConfigurationError
	 *             in case of error
	 * @throws ParserConfigurationException
	 *             in case of error
	 * @throws SAXException
	 *             in case of error
	 * @throws IOException
	 *             in case of error
	 */
	public List<Rule> parse(String uri) throws FactoryConfigurationError,
			ParserConfigurationException, SAXException, IOException {
		SAXParser parser = createParser();
		RulesHandler handler = new RulesHandler();
		parser.parse(uri, handler);
		return handler.getRules();
	}

	/**
	 * Parses the resource
	 * 
	 * @param uri
	 *            the uri
	 * @return the rule list
	 * @throws ParserConfigurationException
	 * @throws FactoryConfigurationError
	 * @throws IOException
	 * @throws SAXException
	 */
	public List<Rule> parse(URL url) throws SAXException, IOException,
			FactoryConfigurationError, ParserConfigurationException {
		RulesHandler handler = new RulesHandler();
		createParser().parse(url.openStream(), handler);
		return handler.getRules();
	}

	/**
	 * Set true to validate the documents created
	 * 
	 * @param isValidating
	 *            true to validate the documents created
	 * @throws FileNotFoundException
	 *             in case the schema dose not exist
	 * @throws SAXException
	 *             in case of schema error
	 */
	public void setValidating(boolean isValidating)
			throws FileNotFoundException, SAXException {
		if (isValidating)
			schemaValidator = createSchema();
		else
			schemaValidator = null;
	}
}