package org.mmarini.fuzzy.parse;

import org.mmarini.fuzzy.VeryExp;

/**
 * @author US00852
 * @version $Id: VeryState.java,v 1.2 2005/02/10 22:32:38 marco Exp $
 */
public class VeryState extends ParserAdapter {

	/**
	 * 
	 * @param handler
	 */
	public VeryState(RulesHandler handler) {
		super(handler);
	}

	/**
	 * @see org.mmarini.fuzzy.parse.ParseState#end(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void end() {
		RulesHandler handler = getHandler();
		handler.pushExp(new VeryExp(handler.pop()));
	}
}