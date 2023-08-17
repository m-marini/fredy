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
import java.util.stream.Collectors;

import static java.lang.Math.max;
import static java.util.Objects.requireNonNull;
import static org.mmarini.yaml.schema.Validator.arrayItems;
import static org.mmarini.yaml.schema.Validator.objectPropertiesRequired;

/**
 * Gets the and value of expressions
 */
public class Or implements InferenceNode {
    public static final Validator JSON_SPEC = objectPropertiesRequired(Map.of(
                    "expressions", arrayItems(InferenceNode.JSON_SPEC)),
            List.of("expressions")
    );

    /**
     * Returns the and node
     *
     * @param nodes the node list
     */
    public static Or create(InferenceNode... nodes) {
        return new Or(List.of(nodes));
    }

    /**
     * Returns the inference node by json spec
     *
     * @param root    the document root
     * @param locator the locator
     */
    public static Or fromJson(JsonNode root, Locator locator) {
        JSON_SPEC.apply(locator).accept(root);
        List<InferenceNode> expressions = InferenceNode.listFromNode(root, locator.path("expressions"));
        return new Or(expressions);
    }

    private final List<InferenceNode> expressions;

    /**
     * Creates the and node
     *
     * @param expressions the expression
     */
    public Or(List<InferenceNode> expressions) {
        this.expressions = requireNonNull(expressions);
    }

    @Override
    public void createDependencies(Set<String> dependencies) {
        expressions.forEach(n -> n.createDependencies(dependencies));
    }

    @Override
    public double evaluate(Model model, Evidences evidences) {
        double result = 0;
        for (InferenceNode expression : expressions) {
            result = max(result, expression.evaluate(model, evidences));
        }
        return result;
    }

    @Override
    public String toString() {
        return "or(" + expressions.stream()
                .map(InferenceNode::toString)
                .collect(Collectors.joining(", ")) + ")";
    }
}
