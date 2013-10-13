package org.mmarini.fuzzy.parse;

import org.mmarini.fuzzy.IfOnlyIfExp;

/**
 * @author US00852
 * @version $Id: IfOnlyIfState.java,v 1.2 2005/02/10 22:32:38 marco Exp $
 */
public class IfOnlyIfState extends ExpressionListState {
	/**
	 * 
	 * @param handler
	 */
	public IfOnlyIfState(RulesHandler handler) {
		super(handler);
	}

	/**
	 * @see org.mmarini.fuzzy.parse.ParseState#end(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void end() {
		RulesHandler handler = getHandler();
		handler.pushExp(new IfOnlyIfExp(handler.popExpList()));
	}
}