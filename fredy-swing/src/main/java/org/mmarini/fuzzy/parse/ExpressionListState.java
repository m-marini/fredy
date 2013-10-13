package org.mmarini.fuzzy.parse;

import org.xml.sax.Attributes;

public class ExpressionListState extends ParserAdapter {

	public ExpressionListState(RulesHandler handler) {
		super(handler);
	}

	/**
	 * @see org.mmarini.fuzzy.parse.ParserAdapter#start(org.xml.sax.Attributes)
	 */
	@Override
	public void start(Attributes attributes) {
		getHandler().pushExpList();
	}

}