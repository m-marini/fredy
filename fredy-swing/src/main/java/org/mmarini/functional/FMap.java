/**
 * 
 */
package org.mmarini.functional;

import java.util.Map;

/**
 * @author us00852
 * 
 */
public interface FMap<K, V> extends Map<K, V> {
	/**
	 * 
	 * @param f
	 * @return
	 */
	public <T, S> FMap<T, S> map(Functor2<FEntry<T, S>, K, V> f);

	/**
	 * 
	 * @param f
	 * @return
	 */
	public <T> FList<T> mapToList(Functor2<T, K, V> f);
}
