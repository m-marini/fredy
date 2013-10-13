/**
 * 
 */
package org.mmarini.fuzzy.parse;

import org.xml.sax.Attributes;

/**
 * @author US00852
 * 
 */
public class ParserAdapter implements ParseState {
	private RulesHandler handler;

	/**
	 * 
	 * @param handler
	 */
	protected ParserAdapter(RulesHandler handler) {
		this.handler = handler;
	}

	/**
	 * @see org.mmarini.fuzzy.parse.ParseState#end()
	 */
	@Override
	public void end() {
	}

	/**
	 * @return the handler
	 */
	protected RulesHandler getHandler() {
		return handler;
	}

	/**
	 * @see org.mmarini.fuzzy.parse.ParseState#start(org.xml.sax.Attributes)
	 */
	@Override
	public void start(Attributes attributes) {
	}
}
