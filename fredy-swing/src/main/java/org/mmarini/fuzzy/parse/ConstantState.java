package org.mmarini.fuzzy.parse;

import org.mmarini.fuzzy.ConstantExp;
import org.mmarini.fuzzy.FuzzyBoolean;
import org.xml.sax.Attributes;

/**
 * @author US00852
 * @version $Id: PredicateState.java,v 1.2 2005/02/10 22:32:38 marco Exp $
 */
public class ConstantState extends ParserAdapter {

	private FuzzyBoolean value;

	/**
	 * 
	 * @param handler
	 */
	public ConstantState(FuzzyBoolean value, RulesHandler handler) {
		super(handler);
		this.value = value;
	}

	/**
	 * @see org.mmarini.fuzzy.parse.ParseState#end(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void end() {
	}

	/**
	 * @see org.mmarini.fuzzy.parse.ParserAdapter#start(org.xml.sax.Attributes)
	 */
	@Override
	public void start(Attributes attributes) {
		String conf = attributes.getValue("confidence");
		FuzzyBoolean v = conf != null ? new FuzzyBoolean(
				Double.parseDouble(conf)) : value;
		getHandler().pushExp(new ConstantExp(v));
	}
}