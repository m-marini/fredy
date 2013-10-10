/**
 * 
 */
package org.mmarini.functional;

/**
 * @author us00852
 * 
 */
public class FEntry<K, V> {
	private K key;
	private V value;

	/**
	 * 
	 * @return
	 */
	public K getKey() {
		return key;
	}

	/**
	 * 
	 * @return
	 */
	public V getValue() {
		return value;
	}

	/**
	 * @param key
	 * @param value
	 */
	public FEntry(K key, V value) {
		this.key = key;
		this.value = value;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((key == null) ? 0 : key.hashCode());
		result = prime * result + ((value == null) ? 0 : value.hashCode());
		return result;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		@SuppressWarnings("unchecked")
		FEntry<K, V> other = (FEntry<K, V>) obj;
		if (key == null) {
			if (other.key != null)
				return false;
		} else if (!key.equals(other.key))
			return false;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append(key).append(" -> ").append(value);
		return builder.toString();
	}
}
