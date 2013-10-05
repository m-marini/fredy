package org.mmarini.fuzzy.parse;

import org.mmarini.fuzzy.AssignTrueCmd;

/**
 * @author US00852
 * @version $Id: AssignState.java,v 1.2 2005/02/10 22:32:38 marco Exp $
 */
public class AssignState extends ParserAdapter {

	/**
	 * 
	 * @param handler
	 */
	public AssignState(RulesHandler handler) {
		super(handler);
	}

	/**
	 * @see org.mmarini.fuzzy.parse.ParseState#end(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void end() {
		RulesHandler handler = getHandler();
		handler.pushAssign(new AssignTrueCmd(handler.getName().toString()));
	}
}