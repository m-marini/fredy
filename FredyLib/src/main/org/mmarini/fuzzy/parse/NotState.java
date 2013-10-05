package org.mmarini.fuzzy.parse;

import java.util.List;

import org.mmarini.fuzzy.IExpression;
import org.mmarini.fuzzy.Not;

/**
 * @author US00852
 * @version $Id: NotState.java,v 1.2 2005/02/10 22:32:37 marco Exp $
 */
public class NotState implements IParseState {

	/**
	 * @see org.mmarini.fuzzy.parse.IParseState#end(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void end(RulesHandler handler) {
		List exps = handler.getExpression();
		int idx = exps.size() - 1;
		IExpression parm1 = (IExpression) exps.get(idx);
		IExpression exp = new Not(parm1);
		handler.getExpression().set(idx, exp);
	}

	/**
	 * @see org.mmarini.fuzzy.parse.IParseState#start(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void start(RulesHandler handler) {
	}
}