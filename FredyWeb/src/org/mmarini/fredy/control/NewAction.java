package org.mmarini.fredy.control;

import java.io.IOException;

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
public class NewAction extends Action {

	public static final String RULES_XML = "/rules.xml";

	/**
	 * @param request
	 * @return @throws FactoryConfigurationError
	 * @throws IOException
	 * @throws SAXException
	 * @throws ParserConfigurationException
	 * @throws FactoryConfigurationError
	 */
	protected SessionHandler createHandler(HttpServletRequest request)
			throws FactoryConfigurationError, ParserConfigurationException,
			SAXException, IOException {
		SessionHandler handler = new SessionHandler();
		handler.setResource(RULES_XML);
		handler.analize();
		return handler;
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

		HttpSession session = request.getSession();
		SessionHandler handler = createHandler(request);
		session.setAttribute(ProcessPostulateAction.SESSION_HANDLER_NAME,
				handler);
		return mapping.findForward("success");
	}
}