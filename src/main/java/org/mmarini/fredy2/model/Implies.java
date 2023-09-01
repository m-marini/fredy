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
import java.util.stream.Stream;

import static java.util.Objects.requireNonNull;
import static org.mmarini.yaml.schema.Validator.objectPropertiesRequired;

/**
 * Gets the implies of expression
 */
public class Implies implements InferenceNode {
    public static final Validator JSON_SPEC = objectPropertiesRequired(Map.of(
                    "antecedent", InferenceNode.JSON_SPEC,
                    "consequent", InferenceNode.JSON_SPEC),
            List.of("antecedent", "consequent")
    );

    /**
     * Returns the inference node by json spec
     *
     * @param root    the document root
     * @param locator the locator
     */
    public static Implies fromJson(JsonNode root, Locator locator) {
        JSON_SPEC.apply(locator).accept(root);
        InferenceNode exp1 = InferenceNode.fromJson(root, locator.path("antecedent"));
        InferenceNode exp2 = InferenceNode.fromJson(root, locator.path("consequent"));
        return new Implies(exp1, exp2);
    }

    private final InferenceNode antecedent;
    private final InferenceNode consequent;

    /**
     * Creates the implies node
     *
     * @param antecedent the first expression
     * @param consequent the second expression
     */
    public Implies(InferenceNode antecedent, InferenceNode consequent) {
        this.antecedent = requireNonNull(antecedent);
        this.consequent = requireNonNull(consequent);
    }

    @Override
    public double evaluate(Model model, Map<String, Double> evidences) {
        double a = antecedent.evaluate(model, evidences);
        double b = consequent.evaluate(model, evidences);
        return Math.max(1 - a, b);
    }

    @Override
    public Stream<String> getDependencies() {
        return Stream.concat(
                antecedent.getDependencies(),
                consequent.getDependencies()
        ).distinct();
    }

    @Override
    public String toString() {
        return "implies(" + antecedent + ", " + consequent + ")";
    }
}
