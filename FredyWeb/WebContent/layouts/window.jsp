<%@ taglib uri="http://jakarta.apache.org/struts/tags-tiles" prefix="tiles"%>
<table border="1" cellspacing="0" cellpadding="1" bordercolor="#000000" align="<tiles:getAsString name='align'/>">
 <tr>
  <td class="wintitle">
   <tiles:get name="title"/>
  </td>
 </tr>
 <tr>
  <td>
   <tiles:get name="body"/>
  </td>
 </tr>
</table>