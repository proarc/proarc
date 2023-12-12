package cz.cas.lib.proarc.common.process.imports.ndk;

public enum StreamFileType {

    MASTER_IMAGE, USER_IMAGE, ALTO, OCR, AMD, PDF, MASTER_AUDIO, USER_AUDIO, SOURCE_AUDIO;


    protected static final String MC_GRP = "MC_IMGGRP";
    protected static final String UC_GRP = "UC_IMGGRP";
    protected static final String ALTO_GRP = "ALTOGRP";
    protected static final String TXT_GRP = "TXTGRP";
    protected static final String AMD_METS_GRP = "TECHMDGRP";
    protected static final String OC_GRP = "OC_EBGRP";
    protected static final String SA_AUDIOGRP = "SA_AUDIOGRP";
    protected static final String UC_AUDIOGRP = "UC_AUDIOGRP";
    protected static final String MC_AUDIOGRP = "MC_AUDIOGRP";


    protected static StreamFileType getFileType(String filegrp) {
        if (MC_GRP.equalsIgnoreCase(filegrp)) {
            return MASTER_IMAGE;
        } else if (UC_GRP.equalsIgnoreCase(filegrp)) {
            return USER_IMAGE;
        } else if (ALTO_GRP.equalsIgnoreCase(filegrp)) {
            return ALTO;
        } else if (TXT_GRP.equalsIgnoreCase(filegrp)) {
            return OCR;
        } else if (AMD_METS_GRP.equalsIgnoreCase(filegrp)) {
            return AMD;
        } else if (OC_GRP.equalsIgnoreCase(filegrp)) {
            return PDF;
        } else if (MC_AUDIOGRP.equalsIgnoreCase(filegrp)) {
            return MASTER_AUDIO;
        } else if (UC_AUDIOGRP.equalsIgnoreCase(filegrp)) {
            return USER_AUDIO;
        } else if (SA_AUDIOGRP.equalsIgnoreCase(filegrp)) {
            return SOURCE_AUDIO;
        }
        throw new IllegalArgumentException("Unknown fileGrp: " + filegrp);
    }
}
