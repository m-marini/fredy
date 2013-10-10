package org.mmarini.functional;

import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * 
 * @author us00852
 * 
 * @param <E>
 */
public class SetImpl<E> implements FSet<E> {
	private Set<E> delegate;

	/**
	 * 
	 */
	public SetImpl() {
		delegate = new HashSet<E>();
	}

	/**
	 * 
	 * @param args
	 */
	public SetImpl(E... args) {
		this();
		for (E e : args)
			add(e);
	}

	/**
	 * 
	 * @param set
	 */
	public SetImpl(SetImpl<E> set) {
		delegate = new HashSet<E>(set);
	}

	/**
	 * 
	 * @param set
	 */
	public SetImpl(Set<E> set) {
		delegate = set;
	}

	/**
	 * 
	 * @param collection
	 */
	public SetImpl(Collection<E> collection) {
		delegate = new HashSet<E>(collection);
	}

	/**
	 * 
	 * @param f
	 * @return
	 */
	public <T> SetImpl<T> map(Functor1<T, E> f) {
		SetImpl<T> l = new SetImpl<T>();
		for (E e : this)
			l.add(f.apply(e));
		return l;
	}

	/**
	 * 
	 * @param f
	 * @return
	 */
	public <T> FList<T> mapToList(Functor1<T, E> f) {
		FList<T> l = new ListImpl<T>();
		for (E e : this)
			l.add(f.apply(e));
		return l;
	}

	/**
	 * 
	 * @param f
	 * @return
	 */
	@Override
	public <K> FMap<K, FSet<E>> groupBy(Functor1<K, E> f) {
		FMap<K, FSet<E>> result = new MapImpl<K, FSet<E>>();
		for (E i : this) {
			K k = f.apply(i);
			FSet<E> s = result.get(k);
			if (s == null) {
				s = new SetImpl<E>();
				result.put(k, s);
			}
			s.add(i);
		}
		return result;
	}

	/**
	 * 
	 * @param c
	 * @return
	 */
	public E min(Comparator<E> c) {
		E m = null;
		for (E i : this)
			if (m == null || c.compare(i, m) < 0)
				m = i;
		return m;
	}

	/**
	 * 
	 * @param f
	 * @return
	 */
	public SetImpl<E> filter(Functor1<Boolean, E> f) {
		SetImpl<E> r = new SetImpl<E>();
		for (E i : this)
			if (f.apply(i))
				r.add(i);
		return r;
	}

	/**
	 * @return
	 * @see java.util.Set#size()
	 */
	public int size() {
		return delegate.size();
	}

	/**
	 * @return
	 * @see java.util.Set#isEmpty()
	 */
	public boolean isEmpty() {
		return delegate.isEmpty();
	}

	/**
	 * @param o
	 * @return
	 * @see java.util.Set#contains(java.lang.Object)
	 */
	public boolean contains(Object o) {
		return delegate.contains(o);
	}

	/**
	 * @return
	 * @see java.util.Set#iterator()
	 */
	public Iterator<E> iterator() {
		return delegate.iterator();
	}

	/**
	 * @return
	 * @see java.util.Set#toArray()
	 */
	public Object[] toArray() {
		return delegate.toArray();
	}

	/**
	 * @param a
	 * @return
	 * @see java.util.Set#toArray(java.lang.Object[])
	 */
	public <T> T[] toArray(T[] a) {
		return delegate.toArray(a);
	}

	/**
	 * @param e
	 * @return
	 * @see java.util.Set#add(java.lang.Object)
	 */
	public boolean add(E e) {
		return delegate.add(e);
	}

	/**
	 * @param o
	 * @return
	 * @see java.util.Set#remove(java.lang.Object)
	 */
	public boolean remove(Object o) {
		return delegate.remove(o);
	}

	/**
	 * @param c
	 * @return
	 * @see java.util.Set#containsAll(java.util.Collection)
	 */
	public boolean containsAll(Collection<?> c) {
		return delegate.containsAll(c);
	}

	/**
	 * @param c
	 * @return
	 * @see java.util.Set#addAll(java.util.Collection)
	 */
	public boolean addAll(Collection<? extends E> c) {
		return delegate.addAll(c);
	}

	/**
	 * @param c
	 * @return
	 * @see java.util.Set#retainAll(java.util.Collection)
	 */
	public boolean retainAll(Collection<?> c) {
		return delegate.retainAll(c);
	}

	/**
	 * @param c
	 * @return
	 * @see java.util.Set#removeAll(java.util.Collection)
	 */
	public boolean removeAll(Collection<?> c) {
		return delegate.removeAll(c);
	}

	/**
	 * 
	 * @see java.util.Set#clear()
	 */
	public void clear() {
		delegate.clear();
	}

	/**
	 * @param o
	 * @return
	 * @see java.util.Set#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		return delegate.equals(o);
	}

	/**
	 * @return
	 * @see java.util.Set#hashCode()
	 */
	public int hashCode() {
		return delegate.hashCode();
	}
}
