/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cas.lib.proarc.webapp.client.ds;

import com.google.gwt.core.client.JavaScriptObject;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.types.DSDataFormat;

/**
 * The common super class to centralize the setup of {@link RestDataSource}.
 *
 * @author Jan Pokorsky
 */
public class ProarcDataSource extends RestDataSource {

    public ProarcDataSource() {
        setDataFormat(DSDataFormat.JSON);
        setJsonPrefix("");
        setJsonSuffix("");
    }

    public ProarcDataSource(JavaScriptObject jsObj) {
        super(jsObj);
    }

}
