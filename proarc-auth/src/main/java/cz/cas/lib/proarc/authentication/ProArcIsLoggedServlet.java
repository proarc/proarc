package cz.cas.lib.proarc.authentication;


import java.io.IOException;
import java.io.PrintWriter;
import java.time.Instant;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

/**
 * Is Logged servlet
 * issue 1712
 *
 * @author Lukas Sykora
 */
public class ProArcIsLoggedServlet extends HttpServlet {

    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        int left = (int) Math.round(req.getSession().getMaxInactiveInterval() - (Instant.now().toEpochMilli() - req.getSession().getLastAccessedTime()) * .001);

        boolean expired = left < 0;

        JSONObject json = new JSONObject();
        try {
            if (expired || req.getSession(false) == null || req.getSession(false).getAttribute("user") == null) {
                json.put("state", "nologged");
                req.getSession();
            } else {
                req.getSession().setMaxInactiveInterval(left);
                Object userData = req.getSession(false).getAttribute("user");
                if (userData != null) {
                    ProarcPrincipal user  = (ProarcPrincipal) userData;
                    if (user != null) {
                        json.put("login", user.getName());
                    }
                }
                json.put("state", "logged");
                json.put("remaining", left);
            }
        } catch (JSONException e) {
            try {
                json.put("error", e.toString());
            } catch (Exception exception) {
                throw new IOException(exception.getMessage(), e);
            }
        }

        resp.setContentType("application/json");
        PrintWriter out = resp.getWriter();
        out.print(json);
        out.flush();
    }
}
