package org.mmarini.fuzzy.handlers;

/**
 * @author US00852
 * @version $Id: HypotesysListHandler.java,v 1.2 2005/02/10 22:32:37 marco Exp $
 */
public class HypotesysListHandler extends AbstractSessionHandlerList {

	/**
	 * @param analisys
	 */
	public HypotesysListHandler(SessionHandler handler) {
		super(handler);
	}

	/**
	 * @see org.mmarini.fuzzy.handlers.AbstractValueList#selectElements()
	 */
	@Override
	public void selectElements() {
		this.setList(this.getHandler().getHypotesys());
	}
}