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
import static org.mmarini.yaml.schema.Validator.string;

/**
 * Get the value og the predicate
 */
public class Predicate implements InferenceNode {
    public static final Validator JSON_SPEC = objectPropertiesRequired(Map.of(
                    "id", string()),
            List.of("id")
    );

    /**
     * Returns the inference node by json spec
     *
     * @param root    the document root
     * @param locator the locator
     */
    static Predicate fromJson(JsonNode root, Locator locator) {
        JSON_SPEC.apply(locator).accept(root);
        return new Predicate(locator.path("id").getNode(root).asText());
    }

    private final String id;

    /**
     * Creates the predicate
     *
     * @param id the identifier
     */
    public Predicate(String id) {
        this.id = requireNonNull(id);
    }

    @Override
    public double evaluate(Model model, Map<String, Double> evidences) {
        Double value = evidences.get(id);
        return value != null
                ? value
                : model.evaluate(id, evidences);
    }

    @Override
    public Stream<String> getDependencies() {
        return Stream.of(id);
    }

    @Override
    public String toString() {
        return "'" + id + "'";
    }
}
