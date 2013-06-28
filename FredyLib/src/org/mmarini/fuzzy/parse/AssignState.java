package org.mmarini.fuzzy.parse;

import org.mmarini.fuzzy.Assign;
import org.mmarini.fuzzy.IPredicate;

/**
 * @author US00852
 * @version $Id: AssignState.java,v 1.2 2005/02/10 22:32:38 marco Exp $
 */
public class AssignState implements IParseState {

	/**
	 * @see org.mmarini.fuzzy.parse.IParseState#end(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void end(RulesHandler handler) {
		IPredicate predicate = handler.getAnalisys().addPredicate(
				handler.getName().toString());
		handler.getAssignExpression().add(new Assign(predicate));
	}

	/**
	 * @see org.mmarini.fuzzy.parse.IParseState#start(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void start(RulesHandler handler) {
		handler.getName().setLength(0);
	}
}