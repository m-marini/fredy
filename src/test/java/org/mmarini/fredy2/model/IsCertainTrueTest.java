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
import java.util.Collection;
import java.util.Map;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mmarini.TestFunctions.text;
import static org.mmarini.yaml.Utils.fromText;
import static org.mockito.Mockito.mock;

class IsCertainTrueTest implements TestUtils {

    @ParameterizedTest
    @CsvSource({
            "0,    0",
            "0.25, 0",
            "0.5 , 0",
            "0.75, 0.5",
            "1   , 1"
    })
    void evaluate(double a, double expected) {
        // Given ...
        IsCertainTrue p = new IsCertainTrue(new Predicate("a"));

        Model model = mock(Model.class);

        Map<String, Double> evidences = Map.of("a", a);

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
                "expression:",
                "  type: predicate",
                "  id: a"
        ));

        // When ...
        IsCertainTrue p = IsCertainTrue.fromJson(node, Locator.root());

        // Then ...
        assertThat(p, hasToString(
                matchesPattern("isCertainTrue\\('a'\\)")
        ));
    }

    @Test
    void getDependencies() {
        // Given ...
        IsCertainTrue p = new IsCertainTrue(new Predicate("a"));

        // When ...
        Collection<String> deps = p.getDependencies().collect(Collectors.toList());

        // Then ...
        assertThat(deps, containsInAnyOrder("a"));
    }
}