/**
 * 
 */
package org.mmarini.fuzzy;

/**
 * @author US00852
 * 
 */
public class IfOnlyIfExp extends AbstractCompositeExp {
	private static final long serialVersionUID = -7778181029612720763L;

	/**
	 * @param parameters
	 */
	public IfOnlyIfExp(Expression... parameters) {
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
			v = v.ifonlyif(v2);
		}
		ctx.push(v);
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("ifOnlyIf").append(super.toString());
		return builder.toString();
	}

}
