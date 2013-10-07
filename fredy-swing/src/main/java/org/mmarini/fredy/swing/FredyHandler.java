/**
 * 
 */
package org.mmarini.fredy.swing;

import java.io.IOException;
import java.net.URL;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;

import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.InferenceEngine;
import org.mmarini.fuzzy.PredicateValue;
import org.xml.sax.SAXException;

/**
 * @author us00852
 * 
 */
public class FredyHandler {

	private InferenceEngine engine;
	private Comparator<? super PredicateValue> cmp;

	/**
	 * 
	 */
	public FredyHandler() {
		engine = new InferenceEngine();
		cmp = new Comparator<PredicateValue>() {

			/**
			 * 
			 * @param p0
			 * @param p1
			 * @return
			 */
			@Override
			public int compare(PredicateValue p0, PredicateValue p1) {
				FuzzyBoolean v0 = p0.getValue();
				FuzzyBoolean v1 = p1.getValue();
				double kd = v0.known().getValue() - v1.known().getValue();
				if (kd < -0.0)
					return -1;
				if (kd > 0.0)
					return 1;
				double vd = v0.getValue() - v1.getValue();
				if (vd < -0.0)
					return -1;
				if (vd > 0.0)
					return 1;
				return p0.getPredicate().compareTo(p1.getPredicate());
			}
		};
	}

	/**
	 * 
	 * @param values
	 */
	public List<PredicateValue> addAxiomsTo(List<PredicateValue> values) {
		engine.addAxiomsTo(values);
		Collections.sort(values, cmp);
		return values;
	}

	/**
	 * 
	 * @param values
	 */
	public List<PredicateValue> addHypothesisTo(List<PredicateValue> values) {
		engine.addHypothesisTo(values);
		Collections.sort(values, Collections.reverseOrder(cmp));
		return values;
	}

	/**
	 * 
	 * @param values
	 * @return
	 */
	public List<PredicateValue> addInferencesTo(List<PredicateValue> values) {
		engine.addInferencesTo(values);
		Collections.sort(values, Collections.reverseOrder(cmp));
		return values;
	}

	/**
	 * 
	 */
	public void analyze() {
		engine.analyze();
	}

	/**
	 * 
	 * @param url
	 * @throws IOException
	 * @throws SAXException
	 * @throws ParserConfigurationException
	 * @throws FactoryConfigurationError
	 */
	public void loadRules(URL url) throws FactoryConfigurationError,
			ParserConfigurationException, SAXException, IOException {
		engine.loadRules(url);
	}

	/**
	 * 
	 * @param predicates
	 */
	public void setAxioms(List<PredicateValue> predicates) {
		if (predicates != null)
			engine.applyPredicates(predicates);
	}

}
