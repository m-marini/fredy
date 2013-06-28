/*
 * Created on 17-nov-2004
 */
package org.mmarini.fuzzy.parse;

import org.mmarini.fuzzy.Rule;

/**
 * @author US00852
 * @version $Id: RuleState.java,v 1.2 2005/02/10 22:32:37 marco Exp $
 */
public class RuleState implements IParseState {

	/**
	 * @see org.mmarini.fuzzy.parse.IParseState#end(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void end(RulesHandler handler) {
		handler.getAnalisys().addRule(handler.getRule());
	}

	/**
	 * @see org.mmarini.fuzzy.parse.IParseState#start(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void start(RulesHandler handler) {
		handler.setRule(new Rule());
	}

}
