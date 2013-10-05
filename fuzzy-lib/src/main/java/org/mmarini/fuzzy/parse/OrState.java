package org.mmarini.fuzzy.parse;

import org.mmarini.fuzzy.OrExp;

/**
 * @author US00852
 * @version $Id: OrState.java,v 1.2 2005/02/10 22:32:38 marco Exp $
 */
public class OrState extends ExpressionListState {

	/**
	 * 
	 * @param handler
	 */
	protected OrState(RulesHandler handler) {
		super(handler);
	}

	/**
	 * @see org.mmarini.fuzzy.parse.ParseState#end(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void end() {
		RulesHandler handler = getHandler();
		handler.pushExp(new OrExp(handler.popExpList()));
	}
}