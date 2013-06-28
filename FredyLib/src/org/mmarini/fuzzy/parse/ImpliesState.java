package org.mmarini.fuzzy.parse;

import java.util.List;

import org.mmarini.fuzzy.IExpression;
import org.mmarini.fuzzy.Implies;

/**
 * @author US00852
 * @version $Id: ImpliesState.java,v 1.2 2005/02/10 22:32:38 marco Exp $
 */
public class ImpliesState implements IParseState {
	/**
	 * @see org.mmarini.fuzzy.parse.IParseState#end(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void end(RulesHandler handler) {
		List exps = handler.getExpression();
		int idx = exps.size() - 2;
		IExpression parm1 = (IExpression) exps.get(idx);
		IExpression parm2 = (IExpression) exps.get(idx + 1);
		exps.remove(idx + 1);
		Implies exp = new Implies(parm1, parm2);
		handler.getExpression().set(idx, exp);
	}

	/**
	 * @see org.mmarini.fuzzy.parse.IParseState#start(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void start(RulesHandler handler) {
	}
}