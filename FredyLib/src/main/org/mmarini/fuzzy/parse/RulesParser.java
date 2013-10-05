package org.mmarini.fuzzy.parse;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URL;

import javax.xml.XMLConstants;
import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.mmarini.fuzzy.Analisys;
import org.xml.sax.InputSource;
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
	protected SAXParser createParser() throws FactoryConfigurationError,
			ParserConfigurationException, SAXException {
		SAXParserFactory factory = SAXParserFactory.newInstance();
		// factory.setSchema(this.getSchemaValidator());
		SAXParser parser = factory.newSAXParser();
		return parser;
	}

	/**
	 * @throws SAXException
	 * @throws FileNotFoundException
	 */
	protected Schema createSchema() throws SAXException, FileNotFoundException {
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
	 * @return Returns the schemaValidator.
	 */
	protected Schema getSchemaValidator() {
		return schemaValidator;
	}

	/**
	 * Parses the source
	 * 
	 * @param source
	 *            the source
	 * @return the analisys
	 * @throws FactoryConfigurationError
	 *             in case of error
	 * @throws ParserConfigurationException
	 *             in case of error
	 * @throws SAXException
	 *             in case of error
	 * @throws IOException
	 *             in case of error
	 */
	public Analisys parse(InputSource source) throws FactoryConfigurationError,
			ParserConfigurationException, SAXException, IOException {
		SAXParser parser = createParser();
		RulesHandler handler = new RulesHandler();
		parser.parse(source, handler);
		return handler.getAnalisys();
	}

	/**
	 * Parses the uri
	 * 
	 * @param uri
	 *            the uri
	 * @return the analisys
	 * @throws FactoryConfigurationError
	 *             in case of error
	 * @throws ParserConfigurationException
	 *             in case of error
	 * @throws SAXException
	 *             in case of error
	 * @throws IOException
	 *             in case of error
	 */
	public Analisys parse(String uri) throws FactoryConfigurationError,
			ParserConfigurationException, SAXException, IOException {
		SAXParser parser = createParser();
		RulesHandler handler = new RulesHandler();
		parser.parse(uri, handler);
		return handler.getAnalisys();
	}

	/**
	 * @param schemaValidator
	 *            The schemaValidator to set.
	 */
	protected void setSchemaValidator(Schema schemaValidator) {
		this.schemaValidator = schemaValidator;
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
			this.setSchemaValidator(this.createSchema());
		else
			this.setSchemaValidator(null);
	}
}