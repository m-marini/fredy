package org.mmarini.fuzzy.parse;

import java.util.List;

import org.mmarini.fuzzy.IExpression;

/**
 * @author US00852
 * @version $Id: IfState.java,v 1.2 2005/02/10 22:32:37 marco Exp $
 */
public class IfState implements IParseState {

	/**
	 * @see org.mmarini.fuzzy.parse.IParseState#end(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void end(RulesHandler handler) {
		List parms = handler.getExpression();
		handler.getRule().setIfExpression(
				(IExpression) handler.getExpression().get(0));
		parms.clear();
	}

	/**
	 * @see org.mmarini.fuzzy.parse.IParseState#start(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void start(RulesHandler handler) {
		handler.getExpression().clear();
	}
}