package cz.cas.lib.proarc.common.export;

import org.apache.commons.configuration.Configuration;

/**
 * Settings for NDK export
 *
 * @author lsykora
 */
public class NdkExportOptions {
    static final String PROP_NDK_AGENT_CREATOR = "export.ndk.agent.creator";
    static final String PROP_NDK_AGENT_ARCHIVIST = "export.ndk.agent.archivist";
    private String creator;
    static private String archivist;
    static private NdkExportOptions options;

    private NdkExportOptions() {
    }

    public static NdkExportOptions getNdkExportOptions(Configuration config) {
        if (config == null && options != null) {
            return options;
        } else if (config != null && options == null) {
            options = new NdkExportOptions();

            String creator = config.getString(PROP_NDK_AGENT_CREATOR);
            if (creator != null && !creator.isEmpty()) {
                options.setCreator(creator);
            }

            String archivist = config.getString(PROP_NDK_AGENT_ARCHIVIST);
            if (archivist != null && !archivist.isEmpty()) {
                options.setArchivist(archivist);
            }
        }
        return options;
    }

    /**
     * Returns the creator organization - used for mets header
     *
     * @return creator
     */
    public String getCreator() {
        return creator;
    }

    /**
     * Sets the creator organization
     *
     * @param creator
     */
    public void setCreator(String creator) {
        this.creator = creator;
    }

    /**
     * Returns the archivist organization - used for mets header
     *
     * @return
     */
    public String getArchivist() {
        return archivist;
    }

    /**
     * Sets the archivist organization
     *
     * @param archivist
     */
    public void setArchivist(String archivist) {
        this.archivist = archivist;
    }

}