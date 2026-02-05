package cz.cas.lib.proarc.authentication;


import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.time.Instant;
import org.json.JSONException;
import org.json.JSONObject;


/**
 * Is Logged servlet
 * issue 1712
 *
 * @author Lukas Sykora
 */
public class ProArcIsLoggedServlet extends HttpServlet {

    public static int MAX_SESSION_INTERVAL = -1;

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
                    ProarcPrincipal user = (ProarcPrincipal) userData;
                    if (user != null) {
                        json.put("login", user.getName());
                    }
                }
                json.put("state", "logged");
                json.put("remaining", left);
                json.put("maximum", getMaxInterval());
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

    public static void resetSession(HttpServletRequest request) {
        request.getSession().setMaxInactiveInterval(getMaxInterval());
    }

    private static int getMaxInterval() {
        if (MAX_SESSION_INTERVAL < 0) {
            int value = AppConfiguration.MAX_SESSION_INTERVAL;
            try {
                AppConfiguration config = AppConfigurationFactory.getInstance().defaultInstance();
                value = config.getMaxSessionTime();
            } catch (Exception ex) {
                // do nothing
            }
            MAX_SESSION_INTERVAL = value;
        }
        return MAX_SESSION_INTERVAL;
    }
}
