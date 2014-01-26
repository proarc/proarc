/*
 * Copyright (C) 2014 Jan Pokorsky
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.export.desa;

import cz.cas.lib.proarc.common.export.desa.sip2desa.SIP2DESATransporter;
import cz.cas.lib.proarc.common.export.desa.sip2desa.nomen.Nomenclatures;
import cz.cas.lib.proarc.common.export.desa.sip2desa.nomen.Nomenclatures.RdCntrls.RdCntrl;
import cz.cas.lib.proarc.common.export.desa.sip2desa.nomen.Nomenclatures.RecCls.RecCl;
import cz.cas.lib.proarc.common.object.ValueMap;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Logger;
import org.apache.commons.configuration.Configuration;

/**
 * Manages DESA service clients and their configurations.
 *
 * @author Jan Pokorsky
 */
public final class DesaServices {

    /**
     * The property name of list of all active DESA service IDs.
     */
    public static final String PROPERTY_DESASERVICES = "desa.services";
    /**
     * The prefix of properties of the given service.
     */
    public static final String PREFIX_DESA = "desa";

    private static final Logger LOG = Logger.getLogger(DesaServices.class.getName());
    private final Configuration config;


    public DesaServices(Configuration config) {
        this.config = config;
    }

    /**
     * Gets value maps of nomenclatures.
     * @param n nomenclature
     * @param pluginId prefix for each added value map
     * @return list of value maps
     */
    public List<ValueMap> getValueMap(Nomenclatures n, String pluginId) {
        ArrayList<ValueMap> maps = new ArrayList<ValueMap>();
        if (n.getRdCntrls() != null) {
            maps.add(new ValueMap<RdCntrl>(pluginId + ".rd-cntrl", n.getRdCntrls().getRdCntrl()));
        }
        if (n.getRecCls() != null) {
            maps.add(new ValueMap<RecCl>(pluginId + ".rec-cl", n.getRecCls().getRecCl()));
        }
        return maps;
    }

    /**
     * Gets nomenclatures.
     * @param dc configuration to query
     * @return nomenclatures or {@code null}
     */
    public Nomenclatures getNomenclatures(DesaConfiguration dc) {
        List<String> acronyms = dc.getNomenclatureAcronyms();
        if (acronyms.isEmpty()) {
            return null;
        } else {
            SIP2DESATransporter t = new SIP2DESATransporter();
            return t.getNomenclatures(dc.toTransporterConfig(), acronyms);
        }
    }

//    public SIP2DESATransporter getTransporter(String serviceId) {
//        return new SIP2DESATransporter();
//    }

    /**
     * Finds a service configuration that accepts the passes model.
     * The configuration must be listed in {@link #PROPERTY_DESASERVICES}.
     *
     * @param model digital object model
     * @return the service configuration or {@code null}
     */
    public DesaConfiguration findConfiguration(MetaModel model) {
        if (model == null) {
            return null;
        }
        for (String sid : config.getStringArray(PROPERTY_DESASERVICES)) {
            DesaConfiguration dc = readConfiguration(config, sid);
            if (dc.getExportModels().contains(model.getPid())) {
                return dc;
            }
        }
        return null;
    }

    /**
     * Finds a service configuration. The configuration must be listed in
     * {@link #PROPERTY_DESASERVICES}.
     *
     * @param serviceId service ID
     * @return the service configuration or {@code null}
     */
    public DesaConfiguration findConfiguration(String serviceId) {
        for (String sid : config.getStringArray(PROPERTY_DESASERVICES)) {
            if (sid.equals(serviceId)) {
                return readConfiguration(config, sid);
            }
        }
        return null;
    }

    /**
     * Finds a configuration that declares any of the passed model IDs.
     * @param modelIds mode IDs to query
     * @return the service configuration or {@code null}
     */
    public DesaConfiguration findConfigurationWithModel(String... modelIds) {
        for (String sid : config.getStringArray(PROPERTY_DESASERVICES)) {
            DesaConfiguration dc = readConfiguration(config, sid);
            List<String> exportModels = dc.getExportModels();
            for (String modelId : modelIds) {
                if (exportModels.contains(modelId)) {
                    return dc;
                }
            }
        }
        return null;
    }

    /**
     * Gets all configurations.
     * @return the list of service configurations
     */
    public List<DesaConfiguration> getConfigurations() {
        return readConfigurations(config);
    }

    private List<DesaConfiguration> readConfigurations(Configuration config) {
        ArrayList<DesaConfiguration> configs = new ArrayList<DesaConfiguration>();
        for (String serviceId : config.getStringArray(PROPERTY_DESASERVICES)) {
            configs.add(readConfiguration(config, serviceId));
        }
        return configs;
    }

    private DesaConfiguration readConfiguration(Configuration config, String serviceId) {
        String servicePrefix = PREFIX_DESA + '.' + serviceId;
        Configuration serviceConfig = config.subset(servicePrefix);
        return new DesaConfiguration(serviceId, servicePrefix, serviceConfig);
    }

    /**
     * The configuration for access to the DESA registry.
     */
    public static final class DesaConfiguration {

        public static final String PROPERTY_USER = "user";
        public static final String PROPERTY_PASSWD = "password";
        public static final String PROPERTY_PRODUCER = "producer";
        public static final String PROPERTY_OPERATOR = "operator";
        public static final String PROPERTY_RESTAPI = "restapi";
        public static final String PROPERTY_WEBSERVICE = "webservice";
        public static final String PROPERTY_EXPORTMODELS = "exportModels";
        public static final String PROPERTY_NOMENCLATUREACRONYMS = "nomenclatureAcronyms";

        private final String serviceId;
        private final String prefix;
        private final Configuration properties;

        public DesaConfiguration(String serviceId, String prefix, Configuration properties) {
            this.serviceId = serviceId;
            this.prefix = prefix;
            this.properties = properties;
        }

        public String getServiceId() {
            return serviceId;
        }

        /**
         * Digital object model IDs accepted by the registry.
         * @return list of IDs
         */
        public List<String> getExportModels() {
            return Arrays.asList(properties.getStringArray(PROPERTY_EXPORTMODELS));
        }

        /**
         * Gets nomenclature acronyms to query {@link Nomenclatures} for value maps from the registry.
         * @return list of acronyms
         */
        public List<String> getNomenclatureAcronyms() {
            return Arrays.asList(properties.getStringArray(PROPERTY_NOMENCLATUREACRONYMS));
        }

        /**
         * Properties required by the {@link SIP2DESATransporter}.
         * @return map of properties
         */
        public HashMap<String, String> toTransporterConfig() {
            HashMap<String, String> tcfg = new HashMap<String, String>();
            tcfg.put("desa." + PROPERTY_USER, properties.getString(PROPERTY_USER));
            putProperty(tcfg, PROPERTY_USER, PROPERTY_PASSWD, PROPERTY_PRODUCER,
                    PROPERTY_OPERATOR, PROPERTY_RESTAPI, PROPERTY_WEBSERVICE);
            // enforce REST file upload
            tcfg.put("desa.rest", Boolean.TRUE.toString());
            tcfg.put("checkMTDPSP", "false");
            return tcfg;
        }

        private void putProperty(HashMap<String, String> tcfg, String... names) {
            for (String name : names) {
                tcfg.put("desa." + name, properties.getString(name));
            }
        }

        @Override
        public String toString() {
            return "DesaConfiguration{" + "serviceId=" + serviceId + ", models=" + getExportModels()
                    + ", acronyms=" + getNomenclatureAcronyms()
                    + ", properties=" + toTransporterConfig() + '}';
        }

    }

}
