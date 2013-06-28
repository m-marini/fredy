<pre>
<%
	Throwable exception = (Throwable) request.getAttribute("org.apache.struts.action.EXCEPTION");
	java.io.PrintWriter pw = new java.io.PrintWriter(out);
	exception.printStackTrace(pw);
	pw.flush();
%>
</pre>