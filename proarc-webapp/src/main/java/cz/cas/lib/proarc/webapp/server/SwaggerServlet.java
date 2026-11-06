package cz.cas.lib.proarc.webapp.server;

import cz.cas.lib.proarc.authentication.Authenticators;
import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import org.json.JSONArray;
import org.json.JSONObject;

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
        response.setCharacterEncoding("UTF-8");
        try (stream) {
            JSONObject specification = new JSONObject(new String(stream.readAllBytes(), StandardCharsets.UTF_8));
            response.getWriter().write(forAuthenticationMode(specification,
                    Authenticators.getInstance().isKeycloak()).toString(2));
        }
    }

    /** Publish only authentication methods enabled by this installation. */
    static JSONObject forAuthenticationMode(JSONObject specification, boolean keycloak) {
        JSONObject schemes = specification.getJSONObject("components").getJSONObject("securitySchemes");
        JSONArray security = new JSONArray();
        if (keycloak) {
            schemes.remove("proarcLogin");
            schemes.remove("cookieAuth");
            security.put(new JSONObject().put("bearerAuth", new JSONArray()));
        } else {
            schemes.remove("bearerAuth");
            security.put(new JSONObject().put("proarcLogin", new JSONArray()));
            security.put(new JSONObject().put("cookieAuth", new JSONArray()));
        }
        specification.put("security", security);
        specification.put("x-proarc-authentication-mode", keycloak ? "keycloak" : "proarc");
        return specification;
    }
}
