package org.mmarini.fuzzy.parse;

import org.mmarini.fuzzy.Expression;
import org.xml.sax.Attributes;

/**
 * @author US00852
 * @version $Id: IfState.java,v 1.2 2005/02/10 22:32:37 marco Exp $
 */
public class IfState extends ParserAdapter {

	/**
	 * 
	 * @param handler
	 */
	protected IfState(RulesHandler handler) {
		super(handler);
	}

	/**
	 * @see org.mmarini.fuzzy.parse.ParseState#end(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void end() {
		RulesHandler handler = getHandler();
		Expression[] parms = handler.popExpList();
		handler.setCondition(parms[0]);
	}

	/**
	 * @see org.mmarini.fuzzy.parse.ParserAdapter#start(org.xml.sax.Attributes)
	 */
	@Override
	public void start(Attributes attributes) {
		getHandler().pushExpList();
	}
}