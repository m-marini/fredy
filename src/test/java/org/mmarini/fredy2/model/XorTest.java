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

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.Mockito;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.junit.jupiter.api.Assertions.assertEquals;

class XorTest implements TestUtils {

    @Test
    void createDependencies() {
        // Given ...
        Xor p = Xor.create(
                new Predicate("a"),
                new Predicate("b"),
                new Predicate("a")
        );

        // When ...
        List<String> deps = p.getDependencies().collect(Collectors.toList());

        // Then ...
        assertThat(deps, containsInAnyOrder("a", "b"));
    }

    @ParameterizedTest
    @CsvSource({
            "   0,    0,    0",
            "   0, 0.25, 0.25",
            "   0,  0.5,  0.5",
            "   0, 0.75, 0.75",
            "   0,    1,    1",
            "0.25,    0, 0.25",
            "0.25, 0.25, 0.25",
            "0.25,  0.5,  0.5",
            "0.25, 0.75, 0.75",
            "0.25,    1, 0.75",
            " 0.5,    0,  0.5",
            " 0.5, 0.25,  0.5",
            " 0.5,  0.5,  0.5",
            " 0.5, 0.75,  0.5",
            " 0.5,    1,  0.5",
            "0.75,    0, 0.75",
            "0.75, 0.25, 0.75",
            "0.75,  0.5,  0.5",
            "0.75, 0.75, 0.25",
            "0.75,    1, 0.25",
            "   1,    0,    1",
            "   1, 0.25, 0.75",
            "   1,  0.5,  0.5",
            "   1, 0.75, 0.25",
            "   1,    1,    0",
    })
    void evaluate(double a, double b, double expected) {
        // Given ...
        Xor p = Xor.create(
                new Predicate("a"),
                new Predicate("b"));

        Model model = Mockito.mock(Model.class);

        Map<String, Double> evidences = Map.of("a", a, "b", b);

        // When ...
        double value = p.evaluate(model, evidences);

        // Then ...
        assertEquals(expected, value);
    }
}