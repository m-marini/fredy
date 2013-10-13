package org.mmarini.functional;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

/**
 * 
 * @author us00852
 * 
 * @param <E>
 */
public class ListImpl<E> implements FList<E> {
	private List<E> delegate;

	/**
	 * 
	 */
	public ListImpl() {
		delegate = new ArrayList<E>();
	}

	/**
	 * 
	 * @param list
	 */
	public ListImpl(Collection<E> list) {
		delegate = new ArrayList<E>(list);
	}

	/**
	 * 
	 * @param args
	 */
	public ListImpl(E... args) {
		for (E e : args)
			add(e);
	}

	/**
	 * 
	 * @param list
	 */
	public ListImpl(List<E> list) {
		delegate = list;
	}

	/**
	 * @param e
	 * @return
	 * @see java.util.List#add(java.lang.Object)
	 */
	@Override
	public boolean add(E e) {
		return delegate.add(e);
	}

	/**
	 * @param index
	 * @param element
	 * @see java.util.List#add(int, java.lang.Object)
	 */
	@Override
	public void add(int index, E element) {
		delegate.add(index, element);
	}

	/**
	 * @param c
	 * @return
	 * @see java.util.List#addAll(java.util.Collection)
	 */
	@Override
	public boolean addAll(Collection<? extends E> c) {
		return delegate.addAll(c);
	}

	/**
	 * @param index
	 * @param c
	 * @return
	 * @see java.util.List#addAll(int, java.util.Collection)
	 */
	@Override
	public boolean addAll(int index, Collection<? extends E> c) {
		return delegate.addAll(index, c);
	}

	/**
	 * 
	 * @see java.util.List#clear()
	 */
	@Override
	public void clear() {
		delegate.clear();
	}

	/**
	 * @param o
	 * @return
	 * @see java.util.List#contains(java.lang.Object)
	 */
	@Override
	public boolean contains(Object o) {
		return delegate.contains(o);
	}

	/**
	 * @param c
	 * @return
	 * @see java.util.List#containsAll(java.util.Collection)
	 */
	@Override
	public boolean containsAll(Collection<?> c) {
		return delegate.containsAll(c);
	}

	/**
	 * @param o
	 * @return
	 * @see java.util.List#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object o) {
		return delegate.equals(o);
	}

	/**
	 * @param index
	 * @return
	 * @see java.util.List#get(int)
	 */
	@Override
	public E get(int index) {
		return delegate.get(index);
	}

	/**
	 * @return
	 * @see java.util.List#hashCode()
	 */
	@Override
	public int hashCode() {
		return delegate.hashCode();
	}

	/**
	 * @param o
	 * @return
	 * @see java.util.List#indexOf(java.lang.Object)
	 */
	@Override
	public int indexOf(Object o) {
		return delegate.indexOf(o);
	}

	/**
	 * @return
	 * @see java.util.List#isEmpty()
	 */
	@Override
	public boolean isEmpty() {
		return delegate.isEmpty();
	}

	/**
	 * @return
	 * @see java.util.List#iterator()
	 */
	@Override
	public Iterator<E> iterator() {
		return delegate.iterator();
	}

	/**
	 * @param o
	 * @return
	 * @see java.util.List#lastIndexOf(java.lang.Object)
	 */
	@Override
	public int lastIndexOf(Object o) {
		return delegate.lastIndexOf(o);
	}

	/**
	 * @return
	 * @see java.util.List#listIterator()
	 */
	@Override
	public ListIterator<E> listIterator() {
		return delegate.listIterator();
	}

	/**
	 * @param index
	 * @return
	 * @see java.util.List#listIterator(int)
	 */
	@Override
	public ListIterator<E> listIterator(int index) {
		return delegate.listIterator(index);
	}

	/**
	 * 
	 * @param l
	 * @param f
	 */
	private <T> void map(Collection<T> l, Functor1<T, E> f) {
		for (E e : this)
			l.add(f.apply(e));
	}

	/**
	 * 
	 * @return
	 */
	@Override
	public <T> ListImpl<T> map(Functor1<T, E> f) {
		ListImpl<T> l = new ListImpl<T>();
		map(l, f);
		return l;
	}

	/**
	 * @see org.mmarini.functional.FList#mapAsSet(org.mmarini.functional.Functor1)
	 */
	@Override
	public <T> FSet<T> mapAsSet(Functor1<T, E> f) {
		FSet<T> r = new SetImpl<T>();
		map(r, f);
		return r;
	}

	/**
	 * @param index
	 * @return
	 * @see java.util.List#remove(int)
	 */
	@Override
	public E remove(int index) {
		return delegate.remove(index);
	}

	/**
	 * @param o
	 * @return
	 * @see java.util.List#remove(java.lang.Object)
	 */
	@Override
	public boolean remove(Object o) {
		return delegate.remove(o);
	}

	/**
	 * @param c
	 * @return
	 * @see java.util.List#removeAll(java.util.Collection)
	 */
	@Override
	public boolean removeAll(Collection<?> c) {
		return delegate.removeAll(c);
	}

	/**
	 * @param c
	 * @return
	 * @see java.util.List#retainAll(java.util.Collection)
	 */
	@Override
	public boolean retainAll(Collection<?> c) {
		return delegate.retainAll(c);
	}

	/**
	 * @param index
	 * @param element
	 * @return
	 * @see java.util.List#set(int, java.lang.Object)
	 */
	@Override
	public E set(int index, E element) {
		return delegate.set(index, element);
	}

	/**
	 * @return
	 * @see java.util.List#size()
	 */
	@Override
	public int size() {
		return delegate.size();
	}

	/**
	 * @param fromIndex
	 * @param toIndex
	 * @return
	 * @see java.util.List#subList(int, int)
	 */
	@Override
	public List<E> subList(int fromIndex, int toIndex) {
		return delegate.subList(fromIndex, toIndex);
	}

	/**
	 * @return
	 * @see java.util.List#toArray()
	 */
	@Override
	public Object[] toArray() {
		return delegate.toArray();
	}

	/**
	 * @param a
	 * @return
	 * @see java.util.List#toArray(java.lang.Object[])
	 */
	@Override
	public <T> T[] toArray(T[] a) {
		return delegate.toArray(a);
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return String.valueOf(delegate);
	}
}
