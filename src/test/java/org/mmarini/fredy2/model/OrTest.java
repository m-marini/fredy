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
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mmarini.ArgumentsGenerator;
import org.mmarini.yaml.schema.Locator;
import org.mockito.Mockito;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Stream;

import static java.lang.Math.max;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mmarini.TestFunctions.text;
import static org.mmarini.yaml.Utils.fromText;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class OrTest {

    static Stream<Arguments> pairValues() {
        return ArgumentsGenerator.createStream(1234,
                ArgumentsGenerator.uniform(0.0, 1.0),
                ArgumentsGenerator.uniform(0.0, 1.0)
        );
    }

    @Test
    void createDependencies() {
        // Given ...
        Or p = Or.create(
                new Predicate("a"),
                new Predicate("b")
        );
        Set<String> deps = new HashSet<>();

        // When ...
        p.createDependencies(deps);

        // Then ...
        assertThat(deps, containsInAnyOrder("a", "b"));
    }

    @ParameterizedTest
    @MethodSource("pairValues")
    void evaluate(double a, double b) {
        // Given ...
        Or p = Or.create(
                new Predicate("a"),
                new Predicate("b"));

        Model model = Mockito.mock(Model.class);

        Evidences evidences = mock(Evidences.class);
        when(evidences.contains("a")).thenReturn(true);
        when(evidences.contains("b")).thenReturn(true);
        when(evidences.get("a")).thenReturn(a);
        when(evidences.get("b")).thenReturn(b);

        // When ...
        double value = p.evaluate(model, evidences);

        // Then ...
        assertEquals(max(a, b), value);
    }

    @Test
    void fromJson() throws IOException {
        // Given ...
        JsonNode node = fromText(text(
                "---",
                "expressions:",
                "- type: predicate",
                "  id: a",
                "- type: predicate",
                "  id: b"
        ));

        // When ...
        Or p = Or.fromJson(node, Locator.root());

        // Then ...
        assertThat(p, hasToString(
                matchesPattern("or\\('a', 'b'\\)")
        ));
    }
}