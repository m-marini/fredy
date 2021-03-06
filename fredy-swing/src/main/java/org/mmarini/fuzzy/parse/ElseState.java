package org.mmarini.fuzzy.parse;

import org.xml.sax.Attributes;

/**
 * @author US00852
 * @version $Id: ThenState.java,v 1.2 2005/02/10 22:32:38 marco Exp $
 */
public class ElseState extends ParserAdapter {

	/**
	 * 
	 * @param handler
	 */
	public ElseState(RulesHandler handler) {
		super(handler);
	}

	/**
	 * @see org.mmarini.fuzzy.parse.ParserAdapter#end()
	 */
	@Override
	public void end() {
		RulesHandler handler = getHandler();
		handler.setElseConsequences(handler.popAssignList());
	}

	/**
	 * @see org.mmarini.fuzzy.parse.ParserAdapter#start(org.xml.sax.Attributes)
	 */
	@Override
	public void start(Attributes attributes) {
		getHandler().pushAssignList();
	}
}