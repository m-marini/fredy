/**
 * 
 */
package org.mmarini.fuzzy;

/**
 * @author US00852
 * 
 */
public interface Command {
	/**
	 * 
	 * @param ctx
	 * @return
	 */
	public abstract void execute(ExecutionContext ctx);
}
