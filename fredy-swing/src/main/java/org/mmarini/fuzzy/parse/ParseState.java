package org.mmarini.fuzzy.parse;

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

/**
 * @author US00852
 * @version $Id: IParseState.java,v 1.2 2005/02/10 22:32:37 marco Exp $
 */
public interface ParseState {

	/**
	 * Processes end
	 */
	public abstract void end() throws SAXParseException;

	/**
	 * Processes start
	 * 
	 * @param attributes
	 */
	public abstract void start(Attributes attributes) throws SAXParseException;
}