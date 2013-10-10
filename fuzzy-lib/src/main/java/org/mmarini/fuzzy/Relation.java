/**
 * 
 */
package org.mmarini.fuzzy;

import org.mmarini.functional.Functor1;

/**
 * @author us00852
 * 
 */
public class Relation {
	private String source;
	private String target;
	public static final Functor1<String, Relation> targetGetter = new Functor1<String, Relation>() {
		@Override
		public String apply(Relation p) {
			return p.getTarget();
		}
	};
	public static final Functor1<String, Relation> sourceGetter = new Functor1<String, Relation>() {

		@Override
		public String apply(Relation r) {
			return r.getSource();
		}
	};

	/**
	 * 
	 */
	public Relation() {
	}

	public Relation(String source, String target) {
		super();
		this.source = source;
		this.target = target;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("Link [source=").append(source).append(", target=")
				.append(target).append("]");
		return builder.toString();
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((source == null) ? 0 : source.hashCode());
		result = prime * result + ((target == null) ? 0 : target.hashCode());
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
		Relation other = (Relation) obj;
		if (source == null) {
			if (other.source != null)
				return false;
		} else if (!source.equals(other.source))
			return false;
		if (target == null) {
			if (other.target != null)
				return false;
		} else if (!target.equals(other.target))
			return false;
		return true;
	}

	/**
	 * @return the source
	 */
	public String getSource() {
		return source;
	}

	/**
	 * @return the target
	 */
	public String getTarget() {
		return target;
	}

}
