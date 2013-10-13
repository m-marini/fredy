/*
 * Created on 17-nov-2004
 */
package org.mmarini.fuzzy.parse;

import org.mmarini.fuzzy.Rule;

/**
 * @author US00852
 * @version $Id: RuleState.java,v 1.2 2005/02/10 22:32:37 marco Exp $
 */
public class RuleState extends ParserAdapter {

	/**
	 * 
	 * @param handler
	 * @return
	 */
	public RuleState(RulesHandler handler) {
		super(handler);
	}

	/**
	 * @see org.mmarini.fuzzy.parse.ParseState#end(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void end() {
		RulesHandler handler = getHandler();
		Rule r = new Rule(handler.getCondition(),
				handler.getThenConsequences(), handler.getElseConsequences());
		handler.addRule(r);
	}
}
