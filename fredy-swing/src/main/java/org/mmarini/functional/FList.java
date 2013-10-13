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

	/**
	 * 
	 * @return
	 */
	public abstract <T> FSet<T> mapAsSet(Functor1<T, E> f);
}
