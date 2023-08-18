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
import java.util.function.BiFunction;
import java.util.stream.Collectors;

import static org.mmarini.yaml.schema.Validator.*;

/**
 * Node tree of inference model
 */
public interface InferenceNode {
    /**
     * Returns the inference node by json spec
     *
     * @param root    the document root
     * @param locator the locator
     */
    static InferenceNode fromJson(JsonNode root, Locator locator) {
        JSON_SPEC.apply(locator).accept(root);
        String type = locator.path("type").getNode(root).asText();
        return BUILDERS.get(type).apply(root, locator);
    }

    /**
     * Returns the list of inference nodes
     *
     * @param root    the document root
     * @param locator the locator
     */
    static List<InferenceNode> listFromNode(JsonNode root, Locator locator) {
        return locator.elements(root)
                .map(loc -> InferenceNode.fromJson(root, loc))
                .collect(Collectors.toList());
    }

    /**
     * Traverses the tree for the dependencies
     *
     * @param dependencies the dependencies
     */
    void createDependencies(Set<String> dependencies);

    /**
     * Returns the value of the inference node
     *
     * @param model     the model
     * @param evidences the evidences
     */
    double evaluate(Model model, Evidences evidences);

    /**
     * The builders
     */
    Map<String, BiFunction<JsonNode, Locator, InferenceNode>> BUILDERS = Map.ofEntries(
            Map.entry("predicate", Predicate::fromJson),
            Map.entry("not", Not::fromJson),
            Map.entry("and", And::fromJson),
            Map.entry("or", Or::fromJson),
            Map.entry("implies", Implies::fromJson),
            Map.entry("iff", Iff::fromJson),
            Map.entry("isParadox", IsParadox::fromJson),
            Map.entry("isTrue", IsTrue::fromJson),
            Map.entry("isCertain", IsCertain::fromJson),
            Map.entry("very", Very::fromJson),
            Map.entry("somewhat", Somewhat::fromJson)
    );

    Validator JSON_SPEC = objectPropertiesRequired(Map.of(
                    "type", string(values(BUILDERS.keySet().stream()
                            .sorted()
                            .toArray(String[]::new)))),
            List.of("type")
    );
}
