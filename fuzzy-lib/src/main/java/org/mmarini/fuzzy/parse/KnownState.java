package org.mmarini.fuzzy.parse;

import org.mmarini.fuzzy.KnownExp;

/**
 * @author US00852
 * @version $Id: KnownState.java,v 1.2 2005/02/10 22:32:37 marco Exp $
 */
public class KnownState extends ParserAdapter {

	/**
	 * 
	 * @param handler
	 */
	public KnownState(RulesHandler handler) {
		super(handler);
	}

	/**
	 * @see org.mmarini.fuzzy.parse.ParseState#end(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void end() {
		RulesHandler handler = getHandler();
		handler.pushExp(new KnownExp(handler.pop()));
	}
}