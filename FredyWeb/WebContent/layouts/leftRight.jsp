<%@ taglib uri="http://jakarta.apache.org/struts/tags-tiles" prefix="tiles"%>
<table border="0">
	<tr>
		<td valign="<tiles:get name='leftVAlign'/>"><tiles:get name="left"/></td>
		<td valign="<tiles:get name='rightVAlign'/>"><tiles:get name="right"/></td>
	</tr>
</table>