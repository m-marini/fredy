package org.mmarini.fredy.control;

import java.io.IOException;
import java.util.Iterator;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.struts.action.Action;
import org.apache.struts.action.ActionForm;
import org.apache.struts.action.ActionForward;
import org.apache.struts.action.ActionMapping;
import org.mmarini.fredy.handlers.SessionHandler;
import org.xml.sax.SAXException;

/**
 * @author US00852
 * @version $Id: CreateInteractionAction.java,v 1.2 2004/11/18 15:12:00 marco
 *          Exp $
 */
public class ProcessPostulateAction extends Action {

	public static final String SESSION_HANDLER_NAME = "analisys";

	/**
	 * @param handler
	 * @param postulateForm
	 * @throws IOException
	 * @throws SAXException
	 * @throws ParserConfigurationException
	 * @throws FactoryConfigurationError
	 */
	protected void analize(SessionHandler handler, PostulateForm postulateForm)
			throws FactoryConfigurationError, ParserConfigurationException,
			SAXException, IOException {
		Map map = postulateForm.getPostulateMap();
		for (Iterator iter = map.keySet().iterator(); iter.hasNext();) {
			String name = (String) iter.next();
			handler.setValue(name, Double.parseDouble((String) map.get(name)));
		}
		handler.analize();
	}

	/**
	 * @see org.apache.struts.action.Action#execute(org.apache.struts.action.ActionMapping,
	 *      org.apache.struts.action.ActionForm,
	 *      javax.servlet.http.HttpServletRequest,
	 *      javax.servlet.http.HttpServletResponse)
	 */
	@Override
	public ActionForward execute(ActionMapping mapping, ActionForm form,
			HttpServletRequest request, HttpServletResponse response)
			throws Exception {

		PostulateForm postulateForm = (PostulateForm) form;
		HttpSession session = request.getSession();
		SessionHandler handler = (SessionHandler) session
				.getAttribute(ProcessPostulateAction.SESSION_HANDLER_NAME);
		analize(handler, postulateForm);
		return mapping.findForward("success");
	}
}