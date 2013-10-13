/**
 * 
 */
package org.mmarini.functional;

/**
 * @author us00852
 * 
 */
public interface Functor2<R, T1, T2> {
	/**
	 * 
	 * @param p1
	 * @param p2
	 * @return
	 */
	public abstract R apply(T1 p1, T2 p2);

}
