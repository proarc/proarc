package cz.cas.lib.proarc.webapp.server;

import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;

public class SwaggerServlet extends HttpServlet {

    private static final String SWAGGER_PAGE = "/WEB-INF/swagger/index.html";
    private static final String OPENAPI_PATH = "/proarc_openapi.json";
    private static final String OPENAPI_RESOURCE = "cz/cas/lib/proarc/webapp/server/rest/proarc_openapi.json";

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        if (OPENAPI_PATH.equals(request.getPathInfo())) {
            writeOpenApi(response);
            return;
        }

        RequestDispatcher dispatcher = request.getRequestDispatcher(SWAGGER_PAGE);
        dispatcher.forward(request, response);
    }

    private void writeOpenApi(HttpServletResponse response) throws IOException {
        InputStream stream = Thread.currentThread()
                .getContextClassLoader()
                .getResourceAsStream(OPENAPI_RESOURCE);
        if (stream == null) {
            response.sendError(HttpServletResponse.SC_NOT_FOUND, "OpenAPI specification not found.");
            return;
        }

        response.setContentType("application/json");
        try (stream) {
            stream.transferTo(response.getOutputStream());
        }
    }
}
