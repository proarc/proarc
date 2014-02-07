/*
 * Copyright (C) 2013 Pavel Stastny
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.authentication.proarc;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.naming.NamingException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.sql.DataSource;

import cz.cas.lib.proarc.authentication.AbstractAuthenticator;
import cz.cas.lib.proarc.authentication.Authenticator;
import cz.cas.lib.proarc.authentication.ProarcPrincipal;
import cz.cas.lib.proarc.common.sql.DbUtils;
import cz.cas.lib.proarc.common.user.UserUtil;


/**
 * ProArc authentication
 * @author pavels
 */
public class ProArcAuthenticator extends AbstractAuthenticator  {

    public static final Logger LOGGER = Logger
            .getLogger(ProArcAuthenticator.class.getName());

    private Map<String, String> dbUserProperties(Connection con, String userName)
            throws SQLException {

        String sql = "select * from tomcat_users where username= ?";
        Map<String, String> properties = new HashMap<String, String>();
        PreparedStatement pstm = null;
        ResultSet rs = null;
        try {
            // TODO: Change it -> empire DB
            pstm = con.prepareStatement(sql);
            pstm.setString(1, userName);
            rs = pstm.executeQuery();
            while (rs.next()) {
                String uname = rs.getString("username");
                String pswd = rs.getString("userpass");
                properties.put(Authenticator.LOGINNAME, uname);
                properties.put(Authenticator.PASSWORD, pswd);
            }
            return properties;
        } finally {
            if (rs != null)
                DbUtils.close(rs);
            if (pstm != null)
                DbUtils.close(pstm);
        }
    }

    @Override
    public boolean authenticate(Map<String, String> loginProperties, HttpServletRequest request, HttpServletResponse response, ProarcPrincipal principal) {
        Connection con = null;
        try {
            DataSource datasource = DbUtils.getProarcSource();
            con = datasource.getConnection();
            Map<String, String> dbProps = dbUserProperties(con,
                    loginProperties.get(Authenticator.LOGINNAME));
            if (!dbProps.isEmpty()) {
                String hashedPswd = dbProps.get(Authenticator.PASSWORD);
                String givenPswd = loginProperties.get(Authenticator.PASSWORD);
                boolean authenticated=  givenPswd != null
                        && hashedPswd.equals(UserUtil.getDigist(givenPswd));
                if (authenticated) {
                    super.associateUserProfile(principal, loginProperties.get(Authenticator.LOGINNAME));
                }
                return authenticated;
            } else {
                // no user found
                return false;
            }
        } catch (NamingException e) {
            LOGGER.log(Level.SEVERE, e.getMessage(), e);
            return false;
        } catch (SQLException e) {
            LOGGER.log(Level.SEVERE, e.getMessage(), e);
            return false;
        } finally {
            DbUtils.close(con);
        }

    }
}
