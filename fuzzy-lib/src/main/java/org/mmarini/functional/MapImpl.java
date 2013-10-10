/**
 * 
 */
package org.mmarini.functional;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author us00852
 * 
 */
public class MapImpl<K, V> implements FMap<K, V> {
	private Map<K, V> delegate;

	/**
	 * @return
	 * @see java.util.Map#size()
	 */
	public int size() {
		return delegate.size();
	}

	/**
	 * @return
	 * @see java.util.Map#isEmpty()
	 */
	public boolean isEmpty() {
		return delegate.isEmpty();
	}

	/**
	 * @param key
	 * @return
	 * @see java.util.Map#containsKey(java.lang.Object)
	 */
	public boolean containsKey(Object key) {
		return delegate.containsKey(key);
	}

	/**
	 * @param value
	 * @return
	 * @see java.util.Map#containsValue(java.lang.Object)
	 */
	public boolean containsValue(Object value) {
		return delegate.containsValue(value);
	}

	/**
	 * @param key
	 * @return
	 * @see java.util.Map#get(java.lang.Object)
	 */
	public V get(Object key) {
		return delegate.get(key);
	}

	/**
	 * @param key
	 * @param value
	 * @return
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public V put(K key, V value) {
		return delegate.put(key, value);
	}

	/**
	 * @param key
	 * @return
	 * @see java.util.Map#remove(java.lang.Object)
	 */
	public V remove(Object key) {
		return delegate.remove(key);
	}

	/**
	 * @param m
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends K, ? extends V> m) {
		delegate.putAll(m);
	}

	/**
	 * 
	 * @see java.util.Map#clear()
	 */
	public void clear() {
		delegate.clear();
	}

	/**
	 * @return
	 * @see java.util.Map#keySet()
	 */
	public Set<K> keySet() {
		return delegate.keySet();
	}

	/**
	 * @return
	 * @see java.util.Map#values()
	 */
	public Collection<V> values() {
		return delegate.values();
	}

	/**
	 * @return
	 * @see java.util.Map#entrySet()
	 */
	public Set<java.util.Map.Entry<K, V>> entrySet() {
		return delegate.entrySet();
	}

	/**
	 * @param o
	 * @return
	 * @see java.util.Map#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		return delegate.equals(o);
	}

	/**
	 * @return
	 * @see java.util.Map#hashCode()
	 */
	public int hashCode() {
		return delegate.hashCode();
	}

	/**
	 * 
	 */
	public MapImpl() {
		delegate = new HashMap<K, V>();

	}

	/**
	 * 
	 * @param f
	 * @return
	 */
	@Override
	public <T> FList<T> mapToList(Functor2<T, K, V> f) {
		FList<T> r = new ListImpl<T>();
		for (Entry<K, V> e : entrySet()) {
			r.add(f.apply(e.getKey(), e.getValue()));
		}
		return r;
	}

	/**
	 * 
	 */
	@Override
	public <T, S> FMap<T, S> map(Functor2<FEntry<T, S>, K, V> f) {
		FMap<T, S> r = new MapImpl<T, S>();
		for (Entry<K, V> e : entrySet()) {
			FEntry<T, S> g = f.apply(e.getKey(), e.getValue());
			r.put(g.getKey(), g.getValue());
		}
		return r;
	}
}
