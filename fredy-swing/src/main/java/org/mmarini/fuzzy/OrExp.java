/**
 * 
 */
package org.mmarini.fuzzy;

/**
 * @author US00852
 * 
 */
public class OrExp extends AbstractCompositeExp {
	private static final long serialVersionUID = -3944067714456521729L;

	/**
	 * @param parameters
	 */
	public OrExp(Expression... parameters) {
		super(parameters);
	}

	/**
	 * @see org.mmarini.fuzzy.Command#execute(org.mmarini.fuzzy.ExecutionContext
	 *      )
	 */
	@Override
	public void execute(ExecutionContext ctx) {
		FuzzyBoolean v = FuzzyBoolean.FALSE;
		for (Command exp : this) {
			exp.execute(ctx);
			FuzzyBoolean v2 = ctx.pop();
			v = v.or(v2);
		}
		ctx.push(v);
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("or").append(super.toString());
		return builder.toString();
	}

}
