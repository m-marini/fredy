/**
 * 
 */
package org.mmarini.fuzzy;

/**
 * @author US00852
 * 
 */
public class AssertFalseCmd extends AbstractAssertCmd {

	/**
	 * @param parameters
	 */
	public AssertFalseCmd(String predicate) {
		super(predicate);
	}

	/**
	 * @see org.mmarini.fuzzy.Command#execute(org.mmarini.fuzzy.ExecutionContext)
	 */
	@Override
	public void execute(ExecutionContext ctx) {
		ctx.assertFalse(getPredicate());
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append(getPredicate()).append("=false");
		return builder.toString();
	}
}
