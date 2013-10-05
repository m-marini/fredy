/**
 * 
 */
package org.mmarini.fuzzy;

/**
 * @author US00852
 * 
 */
public class VeryExp extends AbstractUnaryExp {

	/**
	 * @param parameter
	 */
	public VeryExp(Expression parameter) {
		super(parameter);
	}

	/**
	 * @see org.mmarini.fuzzy.Command#execute(org.mmarini.fuzzy.ExecutionContext
	 *      )
	 */
	@Override
	public void execute(ExecutionContext ctx) {
		getParameter().execute(ctx);
		FuzzyBoolean value = ctx.pop();
		ctx.push(value.very());
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("very(").append(getParameter()).append(")");
		return builder.toString();
	}
}
