package org.mmarini.fuzzy.parse;

/**
 * @author US00852
 * @version $Id: IParseState.java,v 1.2 2005/02/10 22:32:37 marco Exp $
 */
public interface IParseState {

	/**
	 * Processes end
	 * 
	 * @param handler
	 *            the handler
	 */
	public abstract void end(RulesHandler handler);

	/**
	 * Processes start
	 * 
	 * @param handler
	 *            the handler
	 */
	public abstract void start(RulesHandler handler);
}