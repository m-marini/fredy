package org.mmarini.fuzzy.handlers;

/**
 * @author US00852
 * @version $Id: AbstractSessionHandlerList.java,v 1.2 2005/02/10 22:32:37 marco
 *          Exp $
 */
public abstract class AbstractSessionHandlerList extends AbstractValueList {
	protected SessionHandler handler;

	/**
	 * @param handler
	 */
	public AbstractSessionHandlerList(SessionHandler handler) {
		this.handler = handler;
	}

	/**
	 * @return Returns the analisys.
	 */
	public SessionHandler getHandler() {
		return handler;
	}
}