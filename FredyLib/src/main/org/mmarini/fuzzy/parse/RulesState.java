package org.mmarini.fuzzy.parse;

import org.mmarini.fuzzy.Analisys;

/**
 * @author US00852
 * @version $Id: RulesState.java,v 1.2 2005/02/10 22:32:37 marco Exp $
 */
public class RulesState implements IParseState {

	/**
	 * @see org.mmarini.fuzzy.parse.IParseState#end(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void end(RulesHandler handler) {
	}

	/**
	 * @see org.mmarini.fuzzy.parse.IParseState#start(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void start(RulesHandler handler) {
		handler.setAnalisys(new Analisys());
	}

}