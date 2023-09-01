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
import org.mmarini.Tuple2;
import org.mmarini.yaml.Utils;
import org.mmarini.yaml.schema.Locator;
import org.mmarini.yaml.schema.Validator;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import static java.util.Objects.requireNonNull;
import static org.mmarini.yaml.schema.Validator.*;

/**
 * Gets the model and the languages translation
 */
public class Definitions {
    public static final String CURRENT_VERSION = "0.1";
    public static final Validator JSON_SPEC = objectPropertiesRequired(Map.of(
            "version", string(values(CURRENT_VERSION)),
            "model", Model.JSON_SPEC,
            "languages", objectAdditionalProperties(objectAdditionalProperties(string()))
    ), List.of(
            "version", "model"
    ));

    /**
     * Returns the model from file
     *
     * @param file the file
     * @throws IOException in case of error
     */
    public static Definitions fromFile(File file) throws IOException {
        return fromJson(Utils.fromFile(file), Locator.root());
    }

    /**
     * Returns the definitions from json document
     *
     * @param root    the document
     * @param locator the definitions locator
     */
    public static Definitions fromJson(JsonNode root, Locator locator) {
        JSON_SPEC.apply(locator).accept(root);
        Model model = Model.fromJson(root, locator.path("model"));
        Map<String, Properties> languages = languagesFromJson(root, locator.path("languages"));
        return new Definitions(model, languages);
    }

    /**
     * Returns the definitions from a resource
     *
     * @param resource the resource
     * @throws IOException in case of error
     */
    public static Definitions fromResource(String resource) throws IOException {
        return fromJson(Utils.fromResource(resource), Locator.root());
    }

    /**
     * Returns the definition from a resource
     *
     * @param url the resource
     * @throws IOException in case of error
     */
    public static Definitions fromUrl(String url) throws IOException {
        return fromJson(Utils.fromUrl(new URL(url)), Locator.root());
    }

    /**
     * Returns the language properties by language
     *
     * @param root    the document root
     * @param locator the languages locator
     */
    private static Map<String, Properties> languagesFromJson(JsonNode root, Locator locator) {
        return locator.getNode(root).isMissingNode()
                ? Map.of()
                : locator.propertyNames(root)
                .map(t -> {
                    Properties props = new Properties();
                    t._2.propertyNames(root)
                            .forEach(t1 -> props.put(t1._1, t1._2.getNode(root).asText()));
                    return t.setV2(props);
                })
                .collect(Tuple2.toMap());
    }

    private final Model model;
    private final Map<String, Properties> languages;

    /**
     * Creates the definitions
     *
     * @param model     the model
     * @param languages the properties by language
     */
    public Definitions(Model model, Map<String, Properties> languages) {
        this.model = requireNonNull(model);
        this.languages = requireNonNull(languages);
    }

    /**
     * Returns the properties by language
     */
    public Map<String, Properties> getLanguages() {
        return languages;
    }

    /**
     * Returns the model
     */
    public Model getModel() {
        return model;
    }
}
