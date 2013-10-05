package org.mmarini.fredy.control;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.struts.action.ActionForm;
import org.apache.struts.action.ActionForward;
import org.apache.struts.action.ActionMapping;
import org.apache.struts.actions.DispatchAction;
import org.mmarini.fredy.handlers.SessionHandler;

/**
 * @author US00852
 * @version $Id: ChangePageAction.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class ChangePageAction extends DispatchAction {

	public ActionForward firstEvidencePage(ActionMapping mapping,
			ActionForm form, HttpServletRequest request,
			HttpServletResponse response) throws Exception {
		HttpSession session = request.getSession();
		SessionHandler handler = (SessionHandler) session
				.getAttribute(ProcessPostulateAction.SESSION_HANDLER_NAME);
		handler.gotoFirstEvidencePage();
		return mapping.findForward("success");
	}

	/**
	 * Previous hypotesys page
	 * 
	 * @param mapping
	 * @param form
	 * @param request
	 * @param response
	 * @return @throws Exception
	 */
	public ActionForward firstHypotesysPage(ActionMapping mapping,
			ActionForm form, HttpServletRequest request,
			HttpServletResponse response) throws Exception {
		HttpSession session = request.getSession();
		SessionHandler handler = (SessionHandler) session
				.getAttribute(ProcessPostulateAction.SESSION_HANDLER_NAME);
		handler.gotoFirstHypotesysPage();
		return mapping.findForward("success");
	}

	/**
	 * Previous hypotesys page
	 * 
	 * @param mapping
	 * @param form
	 * @param request
	 * @param response
	 * @return @throws Exception
	 */
	public ActionForward firstPostulatePage(ActionMapping mapping,
			ActionForm form, HttpServletRequest request,
			HttpServletResponse response) throws Exception {
		HttpSession session = request.getSession();
		SessionHandler handler = (SessionHandler) session
				.getAttribute(ProcessPostulateAction.SESSION_HANDLER_NAME);
		handler.gotoFirstPostulatePage();
		return mapping.findForward("success");
	}

	public ActionForward lastEvidencePage(ActionMapping mapping,
			ActionForm form, HttpServletRequest request,
			HttpServletResponse response) throws Exception {
		HttpSession session = request.getSession();
		SessionHandler handler = (SessionHandler) session
				.getAttribute(ProcessPostulateAction.SESSION_HANDLER_NAME);
		handler.gotoLastEvidencePage();
		return mapping.findForward("success");
	}

	/**
	 * Next hypotesys page
	 * 
	 * @param mapping
	 * @param form
	 * @param request
	 * @param response
	 * @return @throws Exception
	 */
	public ActionForward lastHypotesysPage(ActionMapping mapping,
			ActionForm form, HttpServletRequest request,
			HttpServletResponse response) throws Exception {
		HttpSession session = request.getSession();
		SessionHandler handler = (SessionHandler) session
				.getAttribute(ProcessPostulateAction.SESSION_HANDLER_NAME);
		handler.gotoLastHypotesysPage();
		return mapping.findForward("success");
	}

	/**
	 * Next hypotesys page
	 * 
	 * @param mapping
	 * @param form
	 * @param request
	 * @param response
	 * @return @throws Exception
	 */
	public ActionForward lastPostulatePage(ActionMapping mapping,
			ActionForm form, HttpServletRequest request,
			HttpServletResponse response) throws Exception {
		HttpSession session = request.getSession();
		SessionHandler handler = (SessionHandler) session
				.getAttribute(ProcessPostulateAction.SESSION_HANDLER_NAME);
		handler.gotoLastPostulatePage();
		return mapping.findForward("success");
	}

	/**
	 * Next hypotesys page
	 * 
	 * @param mapping
	 * @param form
	 * @param request
	 * @param response
	 * @return @throws Exception
	 */
	public ActionForward nextEvidencePage(ActionMapping mapping,
			ActionForm form, HttpServletRequest request,
			HttpServletResponse response) throws Exception {
		HttpSession session = request.getSession();
		SessionHandler handler = (SessionHandler) session
				.getAttribute(ProcessPostulateAction.SESSION_HANDLER_NAME);
		handler.gotoNextEvidencePage();
		return mapping.findForward("success");
	}

	/**
	 * Next hypotesys page
	 * 
	 * @param mapping
	 * @param form
	 * @param request
	 * @param response
	 * @return @throws Exception
	 */
	public ActionForward nextHypotesysPage(ActionMapping mapping,
			ActionForm form, HttpServletRequest request,
			HttpServletResponse response) throws Exception {
		HttpSession session = request.getSession();
		SessionHandler handler = (SessionHandler) session
				.getAttribute(ProcessPostulateAction.SESSION_HANDLER_NAME);
		handler.gotoNextHypotesysPage();
		return mapping.findForward("success");
	}

	/**
	 * Next hypotesys page
	 * 
	 * @param mapping
	 * @param form
	 * @param request
	 * @param response
	 * @return @throws Exception
	 */
	public ActionForward nextPostulatePage(ActionMapping mapping,
			ActionForm form, HttpServletRequest request,
			HttpServletResponse response) throws Exception {
		HttpSession session = request.getSession();
		SessionHandler handler = (SessionHandler) session
				.getAttribute(ProcessPostulateAction.SESSION_HANDLER_NAME);
		handler.gotoNextPostulatePage();
		return mapping.findForward("success");
	}

	/**
	 * Previous hypotesys page
	 * 
	 * @param mapping
	 * @param form
	 * @param request
	 * @param response
	 * @return @throws Exception
	 */
	public ActionForward prevEvidencePage(ActionMapping mapping,
			ActionForm form, HttpServletRequest request,
			HttpServletResponse response) throws Exception {
		HttpSession session = request.getSession();
		SessionHandler handler = (SessionHandler) session
				.getAttribute(ProcessPostulateAction.SESSION_HANDLER_NAME);
		handler.gotoPreviousEvidencePage();
		return mapping.findForward("success");
	}

	/**
	 * Previous hypotesys page
	 * 
	 * @param mapping
	 * @param form
	 * @param request
	 * @param response
	 * @return @throws Exception
	 */
	public ActionForward prevHypotesysPage(ActionMapping mapping,
			ActionForm form, HttpServletRequest request,
			HttpServletResponse response) throws Exception {
		HttpSession session = request.getSession();
		SessionHandler handler = (SessionHandler) session
				.getAttribute(ProcessPostulateAction.SESSION_HANDLER_NAME);
		handler.gotoPreviousHypotesysPage();
		return mapping.findForward("success");
	}

	/**
	 * Previous hypotesys page
	 * 
	 * @param mapping
	 * @param form
	 * @param request
	 * @param response
	 * @return @throws Exception
	 */
	public ActionForward prevPostulatePage(ActionMapping mapping,
			ActionForm form, HttpServletRequest request,
			HttpServletResponse response) throws Exception {
		HttpSession session = request.getSession();
		SessionHandler handler = (SessionHandler) session
				.getAttribute(ProcessPostulateAction.SESSION_HANDLER_NAME);
		handler.gotoPreviousPostulatePage();
		return mapping.findForward("success");
	}
}