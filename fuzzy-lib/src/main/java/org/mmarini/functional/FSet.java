package org.mmarini.functional;

import java.util.Collection;
import java.util.Comparator;
import java.util.Set;

/**
 * 
 * @author us00852
 * 
 * @param <E>
 */
public interface FSet<E> extends Set<E> {

	/**
	 * 
	 * @param f
	 * @return
	 */
	public abstract FSet<E> filter(Functor1<Boolean, E> f);

	/**
	 * 
	 * @param f
	 * @return
	 */
	public abstract <K> FMap<K, FSet<E>> groupBy(Functor1<K, E> f);

	/**
	 * 
	 * @param collection
	 * @return
	 */
	public abstract FSet<E> intersect(Collection<E> collection);

	/**
	 * 
	 * @param f
	 * @return
	 */
	public abstract <T> FSet<T> map(Functor1<T, E> f);

	/**
	 * 
	 * @param f
	 * @return
	 */
	public abstract <T> FList<T> mapToList(Functor1<T, E> f);

	/**
	 * 
	 * @param c
	 * @return
	 */
	public abstract E min(Comparator<E> c);

	/**
	 * 
	 * @param collection
	 * @return
	 */
	public abstract FSet<E> notIn(Collection<E> collection);

	/**
	 * 
	 * @param collection
	 * @return
	 */
	public abstract FSet<E> union(Collection<E> collection);

}
