/*
 * Copyright (c) 2023 Marco Marini, marco.marini@mmarini.org
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 *    END OF TERMS AND CONDITIONS
 */

package org.mmarini.fredy2.model;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

import static java.util.Objects.requireNonNull;

/**
 * Asserts the value of expression to a predicate
 */
public class Assertion {

    private final String id;
    private final InferenceNode expression;

    /**
     * Creates the assignment
     *
     * @param id         the identifier
     * @param expression the expression
     */
    public Assertion(String id, InferenceNode expression) {
        this.id = requireNonNull(id);
        this.expression = requireNonNull(expression);
    }

    /**
     * Applies the assertion to the evidence in the model
     *
     * @param model     the model
     * @param evidences the evidences
     */
    public void apply(Model model, Evidences evidences) {
        if (!evidences.contains(id)) {
            evidences.put(id, expression.evaluate(model, evidences));
        }
    }

    /**
     * Returns the dependencies
     */
    public Set<String> createDependencies() {
        Set<String> result = new HashSet<>();
        expression.createDependencies(result);
        return result;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Assertion that = (Assertion) o;
        return id.equals(that.id);
    }

    /**
     * Returns the expression
     */
    public InferenceNode getExpression() {
        return expression;
    }

    /**
     * Returns the assignment identifier
     */
    public String getId() {
        return id;
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }

    @Override
    public String toString() {
        return "'" + id + "' := " + expression;
    }
}
