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

import com.fasterxml.jackson.databind.JsonNode;
import org.mmarini.yaml.schema.Locator;
import org.mmarini.yaml.schema.Validator;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static java.util.Objects.requireNonNull;
import static org.mmarini.yaml.schema.Validator.objectPropertiesRequired;

/**
 * Gets the implies of expression
 */
public class Implies implements InferenceNode {
    public static final Validator JSON_SPEC = objectPropertiesRequired(Map.of(
                    "expression1", InferenceNode.JSON_SPEC,
                    "expression2", InferenceNode.JSON_SPEC),
            List.of("expression1", "expression2")
    );

    /**
     * Returns the inference node by json spec
     *
     * @param root    the document root
     * @param locator the locator
     */
    public static Implies fromJson(JsonNode root, Locator locator) {
        JSON_SPEC.apply(locator).accept(root);
        InferenceNode exp1 = InferenceNode.fromJson(root, locator.path("expression1"));
        InferenceNode exp2 = InferenceNode.fromJson(root, locator.path("expression2"));
        return new Implies(exp1, exp2);
    }

    private final InferenceNode expression1;
    private final InferenceNode expression2;

    /**
     * Creates the implies node
     *
     * @param expression1 the first expression
     * @param expression2 the second expression
     */
    public Implies(InferenceNode expression1, InferenceNode expression2) {
        this.expression1 = requireNonNull(expression1);
        this.expression2 = requireNonNull(expression2);
    }

    @Override
    public void createDependencies(Set<String> dependencies) {
        expression1.createDependencies(dependencies);
        expression2.createDependencies(dependencies);
    }

    @Override
    public double evaluate(Model model, Evidences evidences) {
        double a = expression1.evaluate(model, evidences);
        double b = expression2.evaluate(model, evidences);
        return Math.min(1 - a + b, 1.);
    }

    @Override
    public String toString() {
        return "implies(" + expression1 + ", " + expression2 + ")";
    }
}
