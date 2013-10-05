package org.mmarini.fuzzy.parse;

import java.util.List;

import org.mmarini.fuzzy.And;

/**
 * @author US00852
 * @version $Id: AndState.java,v 1.2 2005/02/10 22:32:38 marco Exp $
 */
public class AndState implements IParseState {
	private int size;

	/**
	 * @see org.mmarini.fuzzy.parse.IParseState#end(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void end(RulesHandler handler) {
		And exp = new And();
		int oldSize = this.getSize();
		List exps = handler.getExpression();
		int size = exps.size();
		List parms = exps.subList(oldSize, size);
		exp.getParameter().addAll(parms);
		parms.clear();
		handler.getExpression().add(exp);
	}

	/**
	 * @return Returns the list.
	 */
	protected int getSize() {
		return size;
	}

	/**
	 * @param size
	 *            The list to set.
	 */
	protected void setSize(int size) {
		this.size = size;
	}

	/**
	 * @see org.mmarini.fuzzy.parse.IParseState#start(org.mmarini.fuzzy.parse.RulesHandler)
	 */
	@Override
	public void start(RulesHandler handler) {
		this.setSize(handler.getExpression().size());
	}
}