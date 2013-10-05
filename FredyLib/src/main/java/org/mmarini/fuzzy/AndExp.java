/**
 * 
 */
package org.mmarini.fuzzy;

/**
 * @author US00852
 * 
 */
public class AndExp extends AbstractCompositeExp {
	private static final long serialVersionUID = 5183208316991050113L;

	/**
	 * @param parameters
	 */
	public AndExp(Expression... parameters) {
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
			v = v.and(v2);
		}
		ctx.push(v);
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("and").append(super.toString());
		return builder.toString();
	}

}
