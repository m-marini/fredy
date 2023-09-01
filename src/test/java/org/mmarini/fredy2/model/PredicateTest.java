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
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.mmarini.yaml.schema.Locator;

import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mmarini.TestFunctions.text;
import static org.mmarini.yaml.Utils.fromText;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class PredicateTest implements TestUtils {

    @Test
    void createDependencies() {
        // Given ...
        Predicate p = new Predicate("a");

        // When ...
        Collection<String> deps = p.getDependencies().collect(Collectors.toList());

        // Then ...
        assertThat(deps, containsInAnyOrder("a"));
    }

    @ParameterizedTest
    @MethodSource("singleValues")
    void evaluateExits(double a) {
        // Given ...
        Predicate p = new Predicate("a");
        Model model = mock(Model.class);

        Map<String, Double> evidences = Map.of("a", a);

        // When ...
        double value = p.evaluate(model, evidences);

        // Then ...
        assertEquals(a, value);
    }

    @ParameterizedTest
    @MethodSource("singleValues")
    void evaluateNoExits(double a) {
        // Given ...
        Predicate p = new Predicate("a");

        Map<String, Double> evidences = new HashMap<>();
        Model model = mock(Model.class);
        when(model.evaluate("a", evidences)).thenReturn(a);

        // When ...
        double value = p.evaluate(model, evidences);

        // Then ...
        assertEquals(a, value);
    }

    @Test
    void fromJson() throws IOException {
        // Given ...
        JsonNode node = fromText(text(
                "---",
                "id: a"
        ));

        // When ...
        Predicate p = Predicate.fromJson(node, Locator.root());

        // Then ...
        assertThat(p, hasToString(
                matchesPattern("'a'")
        ));
    }
}