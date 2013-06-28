<%@ taglib uri="http://jakarta.apache.org/struts/tags-tiles" prefix="tiles"%>
<%@ taglib uri="http://jakarta.apache.org/struts/tags-html" prefix="html"%>
<%@ taglib uri="http://java.sun.com/jstl/core" prefix="c" %>
<%@ taglib uri="http://java.sun.com/jstl/fmt" prefix="fmt" %>
<fmt:setLocale value="${sessionScope['org.apache.struts.action.LOCALE']}"/>
<div align="center">
</div>
<table border="1" cellspacing="0" cellpadding="1">
	<c:forEach var="predicate" items="${evidence.elements}">
		<c:choose>
			<c:when test="${predicate.value.value < 0.5}">
				<c:set var="classValue" value="false"/>
			</c:when>
			<c:when test="${predicate.value.value > 0.5}">
				<c:set var="classValue" value="true"/>
			</c:when>
			<c:otherwise>
				<c:set var="classValue" value="unknown"/>
			</c:otherwise>
		</c:choose>
		<tr class="<c:out value='${classValue}'/>">
			<td><fmt:message key="text.predicate.${predicate.name}"/></td>
			<td>
				<fmt:message key="text.value.${predicate.value.description}"/>
			</td>
			<td>
				<fmt:formatNumber value="${predicate.value.value}" type="percent"/>
			</td>
		</tr>
	</c:forEach>
</table>
<tiles:insert definition="scrollBar.layout">
	<tiles:put name="handler" beanName="evidence"/>
	<tiles:put name="suffix" value="EvidencePage"/>
</tiles:insert>