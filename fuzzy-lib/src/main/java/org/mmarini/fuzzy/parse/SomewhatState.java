package org.mmarini.fuzzy.parse;

import org.mmarini.fuzzy.SomewhatExp;

/**
 * @author US00852
 * @version $Id: SomewhatState.java,v 1.2 2005/02/10 22:32:38 marco Exp $
 */
public class SomewhatState extends ParserAdapter {

	/**
	 * 
	 * @param handler
	 */
	public SomewhatState(RulesHandler handler) {
		super(handler);
	}

	/**
	 * @see org.mmarini.fuzzy.parse.ParseState#end(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void end() {
		RulesHandler handler = getHandler();
		handler.pushExp(new SomewhatExp(handler.pop()));
	}
}