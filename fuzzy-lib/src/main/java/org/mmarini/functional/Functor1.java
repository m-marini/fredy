/**
 * 
 */
package org.mmarini.functional;

/**
 * @author us00852
 * 
 */
public interface Functor1<R, P1> {
	/**
	 * 
	 * @param p
	 * @return
	 */
	public abstract R apply(P1 p);
}
