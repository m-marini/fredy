package org.mmarini.fuzzy.parse;

import java.util.List;

/**
 * @author US00852
 * @version $Id: ThenState.java,v 1.2 2005/02/10 22:32:38 marco Exp $
 */
public class ThenState implements IParseState {

	/**
	 * @see org.mmarini.fuzzy.parse.IParseState#end(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void end(RulesHandler handler) {
		List parms = handler.getAssignExpression();
		handler.getRule().addThenExpression(parms);
		parms.clear();
	}

	/**
	 * @see org.mmarini.fuzzy.parse.IParseState#start(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void start(RulesHandler handler) {
		handler.getAssignExpression().clear();
	}
}