<%@ taglib uri="http://jakarta.apache.org/struts/tags-html" prefix="html"%>
<%@ taglib uri="http://jakarta.apache.org/struts/tags-tiles" prefix="tiles"%>
<%@ taglib uri="http://java.sun.com/jstl/core" prefix="c" %>
<%@ taglib uri="http://java.sun.com/jstl/fmt" prefix="fmt" %>
<fmt:setLocale value="${sessionScope['org.apache.struts.action.LOCALE']}"/>
<html:form action="processPostulate">
	<table border="1" cellspacing="0" cellpadding="1">
		<tr>
			<th>&nbsp;</th>
			<th width="10%"><fmt:message key="text.value.false"/></th>
			<th width="10%"><fmt:message key="text.value.quite.false"/></th>
			<th width="10%"><fmt:message key="text.value.unknown"/></th>
			<th width="10%"><fmt:message key="text.value.quite.true"/></th>
			<th width="10%"><fmt:message key="text.value.true"/></th>
		</tr>
		<c:forEach var="predicate" items="${postulate.elements}">
			<c:set var="name" value="${predicate.name}"/>
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
				<td align="center">
					<c:choose>
						<c:when test="${predicate.value.value <= 0.0}">
							<input type="radio" name='<c:out value="postulate(${name})"/>' value="0" checked="true"/>
						</c:when>
						<c:otherwise>
							<input type="radio" name='<c:out value="postulate(${name})"/>' value="0"/>
						</c:otherwise>
					</c:choose>
					</input>
				</td>
				<td align="center">
					<c:choose>
						<c:when test="${predicate.value.quiteFalse}">
							<input type="radio" name='<c:out value="postulate(${name})"/>' value="0.25"  checked="true"/>
						</c:when>
						<c:otherwise>
							<input type="radio" name='<c:out value="postulate(${name})"/>' value="0.25"/>
						</c:otherwise>
					</c:choose>
				</td>
				<td align="center">
					<c:choose>
						<c:when test="${predicate.value.unknown}">
							<input type="radio" name='<c:out value="postulate(${name})"/>' value="0.5"  checked="true"/>
						</c:when>
						<c:otherwise>
							<input type="radio" name='<c:out value="postulate(${name})"/>' value="0.5"/>
						</c:otherwise>
					</c:choose>
				</td>
				<td align="center">
					<c:choose>
						<c:when test="${predicate.value.quiteTrue}">
							<input type="radio" name='<c:out value="postulate(${name})"/>' value="0.75"  checked="true"/>
						</c:when>
						<c:otherwise>
							<input type="radio" name='<c:out value="postulate(${name})"/>' value="0.75"/>
						</c:otherwise>
					</c:choose>
				</td>
				<td align="center">
					<c:choose>
						<c:when test="${predicate.value.value >= 1.}">
							<input type="radio" name='<c:out value="postulate(${name})"/>' value="1"  checked="true"/>
						</c:when>
						<c:otherwise>
							<input type="radio" name='<c:out value="postulate(${name})"/>' value="1"/>
						</c:otherwise>
					</c:choose>
				</td>
			</tr>
		</c:forEach>
	</table>
	<table align="center" border="0" cellspacing="0" cellpadding="4">
		<tr>
			<td>
				<a href="#" onclick="postulateForm.submit();return 0">
					<fmt:message key="text.link.analize"/>
				</a>
			</td>
			<td><a href="newAnalisys.do"><fmt:message key="text.link.new"/></a></td>
		</tr>
	</table>
</html:form>
<tiles:insert definition="scrollBar.layout">
	<tiles:put name="handler" beanName="postulate"/>
	<tiles:put name="suffix" value="PostulatePage"/>
</tiles:insert>