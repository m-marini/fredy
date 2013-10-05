package org.mmarini.fuzzy.parse;

import org.mmarini.fuzzy.IPredicate;
import org.mmarini.fuzzy.NotAssign;

/**
 * @author US00852
 * @version $Id: AssignNotState.java,v 1.2 2005/02/10 22:32:37 marco Exp $
 */
public class AssignNotState implements IParseState {

	/**
	 * @see org.mmarini.fuzzy.parse.IParseState#end(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void end(RulesHandler handler) {
		IPredicate predicate = handler.getAnalisys().addPredicate(
				handler.getName().toString());
		handler.getAssignExpression().add(new NotAssign(predicate));
	}

	/**
	 * @see org.mmarini.fuzzy.parse.IParseState#start(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void start(RulesHandler handler) {
		handler.getName().setLength(0);
	}
}