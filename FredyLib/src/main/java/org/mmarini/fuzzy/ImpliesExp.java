/**
 * 
 */
package org.mmarini.fuzzy;

/**
 * @author US00852
 * 
 */
public class ImpliesExp extends AbstractCompositeExp {
	private static final long serialVersionUID = 1795092504899327740L;

	/**
	 * @param parameters
	 */
	public ImpliesExp(Expression... parameters) {
		super(parameters);
	}

	/**
	 * @see org.mmarini.fuzzy.Command#execute(org.mmarini.fuzzy.ExecutionContext
	 *      )
	 */
	@Override
	public void execute(ExecutionContext ctx) {
		FuzzyBoolean v = FuzzyBoolean.TRUE;
		for (Command exp : this) {
			exp.execute(ctx);
			FuzzyBoolean v2 = ctx.pop();
			v = v.implies(v2);
		}
		ctx.push(v);
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("implies").append(super.toString());
		return builder.toString();
	}

}
