<%@ taglib uri="http://jakarta.apache.org/struts/tags-html" prefix="html"%>
<%@ taglib uri="http://jakarta.apache.org/struts/tags-tiles" prefix="tiles"%>
<%@ taglib uri="http://java.sun.com/jstl/core" prefix="c" %>
<%@ taglib uri="http://java.sun.com/jstl/fmt" prefix="fmt" %>
<fmt:setLocale value="${sessionScope['org.apache.struts.action.LOCALE']}"/>
<tiles:importAttribute/>
<table align="center" border="0" cellspacing="0" cellpadding="2">
	<tr>
		<td>
			<c:if test="${handler.pageNumber > 1}">
				<a href="<c:url value='changePage.do'>
						<c:param name='action' value='first${suffix}'/>
					</c:url>">
				<fmt:message key="text.first.page"/>
				</a>
			</c:if>
		</td>
		<td>
			<c:if test="${handler.pageNumber > 1}">
				<a href="<c:url value='changePage.do'>
						<c:param name='action' value='prev${suffix}'/>
					</c:url>">
				<fmt:message key="text.prev.page"/>
				</a>
			</c:if>
		</td>
		<td colspan="2" align="center">
			<fmt:message key="text.page">
				<fmt:param value="${handler.pageNumber}"/>
				<fmt:param value="${handler.pageCount}"/>
			</fmt:message>
		</td>
		<td align="right">
			<c:if test="${handler.pageNumber < handler.pageCount}">
				<a href="<c:url value='changePage.do'>
						<c:param name='action' value='next${suffix}'/>
					</c:url>">
				<fmt:message key="text.next.page"/>
				</a>
			</c:if>
		</td>
		<td align="right">
			<c:if test="${handler.pageNumber < handler.pageCount}">
				<a href="<c:url value='changePage.do'>
						<c:param name='action' value='last${suffix}'/>
					</c:url>">
				<fmt:message key="text.last.page"/>
				</a>
			</c:if>
		</td>
	</tr>
</table>