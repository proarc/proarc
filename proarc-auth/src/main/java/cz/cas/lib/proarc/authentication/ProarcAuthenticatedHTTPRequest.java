package cz.cas.lib.proarc.authentication;

import jakarta.servlet.ServletRequest;
import jakarta.servlet.http.HttpServletRequest;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

public class ProarcAuthenticatedHTTPRequest implements InvocationHandler {

    private HttpServletRequest reqest;
    private ProarcPrincipal userPrincipal;
    private String remoteUser;

    ProarcAuthenticatedHTTPRequest(HttpServletRequest request,
                                   ProarcPrincipal uPrincipal, String rUser) {
        super();
        this.reqest = request;
        this.userPrincipal = uPrincipal;
        this.remoteUser = rUser;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args)
            throws Throwable {
        if (method.getName().equals("getRemoteUser")) {
            return this.remoteUser;
        } else if (method.getName().equals("getUserPrincipal")) {
            return this.userPrincipal;
        } else {
            return method.invoke(this.reqest, args);
        }
    }

    public static HttpServletRequest newInstance(HttpServletRequest reqest,
                                                 ProarcPrincipal uPrincipal, String rUser) {
        return (HttpServletRequest) java.lang.reflect.Proxy.newProxyInstance(
                ProarcAuthenticatedHTTPRequest.class.getClassLoader(),
                new Class[]{ServletRequest.class, HttpServletRequest.class},
                new ProarcAuthenticatedHTTPRequest(reqest, uPrincipal, rUser));
    }
}
