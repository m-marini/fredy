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
	 * 
	 */
	public MapImpl() {
		delegate = new HashMap<K, V>();

	}

	/**
	 * 
	 * @see java.util.Map#clear()
	 */
	@Override
	public void clear() {
		delegate.clear();
	}

	/**
	 * @param key
	 * @return
	 * @see java.util.Map#containsKey(java.lang.Object)
	 */
	@Override
	public boolean containsKey(Object key) {
		return delegate.containsKey(key);
	}

	/**
	 * @param value
	 * @return
	 * @see java.util.Map#containsValue(java.lang.Object)
	 */
	@Override
	public boolean containsValue(Object value) {
		return delegate.containsValue(value);
	}

	/**
	 * @return
	 * @see java.util.Map#entrySet()
	 */
	@Override
	public Set<java.util.Map.Entry<K, V>> entrySet() {
		return delegate.entrySet();
	}

	/**
	 * @param o
	 * @return
	 * @see java.util.Map#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object o) {
		return delegate.equals(o);
	}

	/**
	 * @param key
	 * @return
	 * @see java.util.Map#get(java.lang.Object)
	 */
	@Override
	public V get(Object key) {
		return delegate.get(key);
	}

	/**
	 * @return
	 * @see java.util.Map#hashCode()
	 */
	@Override
	public int hashCode() {
		return delegate.hashCode();
	}

	/**
	 * @return
	 * @see java.util.Map#isEmpty()
	 */
	@Override
	public boolean isEmpty() {
		return delegate.isEmpty();
	}

	/**
	 * @return
	 * @see java.util.Map#keySet()
	 */
	@Override
	public Set<K> keySet() {
		return delegate.keySet();
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
	 * @param key
	 * @param value
	 * @return
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	@Override
	public V put(K key, V value) {
		return delegate.put(key, value);
	}

	/**
	 * @param m
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	@Override
	public void putAll(Map<? extends K, ? extends V> m) {
		delegate.putAll(m);
	}

	/**
	 * @param key
	 * @return
	 * @see java.util.Map#remove(java.lang.Object)
	 */
	@Override
	public V remove(Object key) {
		return delegate.remove(key);
	}

	/**
	 * @return
	 * @see java.util.Map#size()
	 */
	@Override
	public int size() {
		return delegate.size();
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return String.valueOf(delegate);
	}

	/**
	 * @return
	 * @see java.util.Map#values()
	 */
	@Override
	public Collection<V> values() {
		return delegate.values();
	}
}
