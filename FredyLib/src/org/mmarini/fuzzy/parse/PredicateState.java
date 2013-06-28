package org.mmarini.fuzzy.parse;

import org.mmarini.fuzzy.IPredicate;
import org.mmarini.fuzzy.PredicateExpression;

/**
 * @author US00852
 * @version $Id: PredicateState.java,v 1.2 2005/02/10 22:32:38 marco Exp $
 */
public class PredicateState implements IParseState {

	/**
	 * @see org.mmarini.fuzzy.parse.IParseState#end(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void end(RulesHandler handler) {
		IPredicate predicate = handler.getAnalisys().addPredicate(
				handler.getName().toString());
		handler.getExpression().add(new PredicateExpression(predicate));
	}

	/**
	 * @see org.mmarini.fuzzy.parse.IParseState#start(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void start(RulesHandler handler) {
		handler.getName().setLength(0);
	}
}