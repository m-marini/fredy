package org.mmarini.fredy.control;

import java.util.Iterator;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.struts.action.Action;
import org.apache.struts.action.ActionForm;
import org.apache.struts.action.ActionForward;
import org.apache.struts.action.ActionMapping;
import org.mmarini.fredy.handlers.SessionHandler;
import org.mmarini.fuzzy.IPredicate;

/**
 * @author US00852
 * @version $Id: CreateInteractionAction.java,v 1.2 2004/11/18 15:12:00 marco
 *          Exp $
 */
public class ShowAction extends Action {
	public static final String POSTULATE_NAME = "postulate";
	public static final String HYPOTESYS_NAME = "hypotesys";
	public static final String EVIDENCE_NAME = "evidence";

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
		this.processPostulates(postulateForm, handler, request);
		this.processHypotesys(handler, request);
		this.processEvidences(handler, request);
		return mapping.findForward("success");
	}

	/**
	 * @param handler
	 * @param request
	 */
	protected void processEvidences(SessionHandler handler,
			HttpServletRequest request) {
		request.setAttribute(EVIDENCE_NAME, handler.getEvidenceHandler());
	}

	/**
	 * @param handler
	 * @param request
	 */
	protected void processHypotesys(SessionHandler handler,
			HttpServletRequest request) {
		request.setAttribute(HYPOTESYS_NAME, handler.getHypotesysHandler());
	}

	/**
	 * @param postulateForm
	 * @param handler
	 * @param request
	 */
	protected void processPostulates(PostulateForm postulateForm,
			SessionHandler handler, HttpServletRequest request) {
		postulateForm.getPostulateMap().clear();
		List postulate = handler.getPostulate();
		for (Iterator iter = postulate.iterator(); iter.hasNext();) {
			IPredicate predicate = (IPredicate) iter.next();
			postulateForm.setPostulate(predicate.getName(),
					String.valueOf(predicate.getValue().getValue()));
		}
		request.setAttribute(POSTULATE_NAME, handler.getPostulateHandler());
	}
}