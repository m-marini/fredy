<%@ taglib uri="http://jakarta.apache.org/struts/tags-html" prefix="html" %>
<%@ taglib uri="http://jakarta.apache.org/struts/tags-tiles" prefix="tiles"%>
<%@ page contentType="text/html; charset=windows-1252" %>
<%
  response.setHeader("Cache-Control", "no-Cache");
  response.setHeader("Pragma", "No-cache");
  response.setDateHeader("Expires", 0);
%>
<html:html>
	<head>
  	<title><tiles:get name="title"/></title>
	  <link rel="stylesheet" href="style.css" type="text/css">
	</head>
	<body>
		<table border="0" cellspacing="0" cellpadding="4" width="100%">
			<tr><td><tiles:get name="menu"/></td></tr>
			<tr><td>&nbsp;</td></tr>
			<tr><td><tiles:get name="body"/></td></tr>
		</table>
	</body>
</html:html>