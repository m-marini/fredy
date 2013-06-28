<%@ taglib uri="http://jakarta.apache.org/struts/tags-tiles" prefix="tiles"%>
<%@ taglib uri="http://java.sun.com/jstl/fmt" prefix="fmt" %>
<fmt:setLocale value="${sessionScope['org.apache.struts.action.LOCALE']}"/>
<script language="JavaScript">
<!--
function changeVisibility(element) {
 if (element.style.display=='none') {
  element.style.display='block';
 } else {
  element.style.display='none';
 }
}
//-->
 </script>
<p>
<table>
 <tr><td align="center">
  <tiles:get name="errorMessage"/>
 </td></tr>
 <tr><td align="center">
	<a href="javascript:changeVisibility(details);">
	<fmt:message key="button.details.error.general"/>
 </td></tr>
</table>
</p>
<p>
<table>
 <tr><td id="details" style="display: none">
  <tiles:get name="errorDetails"/>
 </td></tr>
</table>
</p>