package cz.cas.lib.proarc.webapp.client.widget;

import cz.cas.lib.proarc.webapp.client.ClientMessages;
import java.util.LinkedHashMap;
import static cz.cas.lib.proarc.common.object.DigitalObjectState.STATUS_ASSIGN;
import static cz.cas.lib.proarc.common.object.DigitalObjectState.STATUS_CONNECTED;
import static cz.cas.lib.proarc.common.object.DigitalObjectState.STATUS_DESCRIBED;
import static cz.cas.lib.proarc.common.object.DigitalObjectState.STATUS_EXPORTED;
import static cz.cas.lib.proarc.common.object.DigitalObjectState.STATUS_NEW;
import static cz.cas.lib.proarc.common.object.DigitalObjectState.STATUS_PROCESSING;

public class DigitalObjectState {

    public static LinkedHashMap<String, String> getMap(ClientMessages i18n) {
        LinkedHashMap<String, String> valueMap = new LinkedHashMap();
        valueMap.put(STATUS_NEW, i18n.DigitalObjectEditor_AdministrationEditor_Status_New_Title());
        valueMap.put(STATUS_ASSIGN, i18n.DigitalObjectEditor_AdministrationEditor_Status_Assign_Title());
        valueMap.put(STATUS_CONNECTED, i18n.DigitalObjectEditor_AdministrationEditor_Status_Connected_Title());
        valueMap.put(STATUS_PROCESSING, i18n.DigitalObjectEditor_AdministrationEditor_Status_Processing_Title());
        valueMap.put(STATUS_DESCRIBED, i18n.DigitalObjectEditor_AdministrationEditor_Status_Described_Title());
        valueMap.put(STATUS_EXPORTED, i18n.DigitalObjectEditor_AdministrationEditor_Status_Exported_Title());
        return valueMap;
    }
}
