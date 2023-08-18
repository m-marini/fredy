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
import org.junit.jupiter.params.provider.CsvSource;
import org.mmarini.yaml.schema.Locator;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mmarini.TestFunctions.text;
import static org.mmarini.yaml.Utils.fromText;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class IffTest implements TestUtils {

    @Test
    void createDependencies() {
        // Given ...
        Iff p = new Iff(
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
    @CsvSource({
            "   0,    0,    1",
            "   0, 0.25, 0.75",
            "   0,  0.5,  0.5",
            "   0, 0.75, 0.25",
            "   0,    1,    0",
            "0.25,    0, 0.75",
            "0.25, 0.25,    1",
            "0.25,  0.5, 0.75",
            "0.25, 0.75,  0.5",
            "0.25,    1, 0.25",
            " 0.5,    0,  0.5",
            " 0.5, 0.25, 0.75",
            " 0.5,  0.5,    1",
            " 0.5, 0.75, 0.75",
            " 0.5,    1,  0.5",
            "0.75,    0, 0.25",
            "0.75, 0.25,  0.5",
            "0.75,  0.5, 0.75",
            "0.75, 0.75,    1",
            "0.75,    1, 0.75",
            "   1,    0,    0",
            "   1, 0.25, 0.25",
            "   1,  0.5,  0.5",
            "   1, 0.75, 0.75",
            "   1,    1,    1",
    })
    void evaluate(double a, double b, double expected) {
        // Given ...
        Iff p = new Iff(
                new Predicate("a"),
                new Predicate("b"));

        Model model = mock(Model.class);

        Evidences evidences = mock(Evidences.class);
        when(evidences.contains("a")).thenReturn(true);
        when(evidences.contains("b")).thenReturn(true);
        when(evidences.get("a")).thenReturn(a);
        when(evidences.get("b")).thenReturn(b);

        // When ...
        double value = p.evaluate(model, evidences);

        // Then ...
        assertEquals(expected, value);
    }

    @Test
    void fromJson() throws IOException {
        // Given ...
        JsonNode node = fromText(text(
                "---",
                "expression1:",
                "  type: predicate",
                "  id: a",
                "expression2:",
                "  type: predicate",
                "  id: b"
        ));

        // When ...
        Iff p = Iff.fromJson(node, Locator.root());

        // Then ...
        assertThat(p, hasToString(
                matchesPattern("iff\\('a', 'b'\\)")
        ));
    }
}