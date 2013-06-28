<%@ taglib uri="http://jakarta.apache.org/struts/tags-tiles" prefix="tiles"%>
<table border="0">
	<tr>
		<td valign="<tiles:get name='upVAlign'/>"><tiles:get name="up"/></td>
	<tr>
	</tr>
		<td valign="<tiles:get name='downVAlign'/>"><tiles:get name="down"/></td>
	</tr>
</table>