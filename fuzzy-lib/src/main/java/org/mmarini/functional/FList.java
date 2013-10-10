package org.mmarini.functional;

import java.util.List;

/**
 * 
 * @author us00852
 * 
 * @param <E>
 */
public interface FList<E> extends List<E> {

	/**
	 * 
	 * @return
	 */
	public abstract <T> FList<T> map(Functor1<T, E> f);
}
