package cz.cas.lib.proarc.common.export.mets.structure;

import cz.cas.lib.proarc.common.export.mets.Const;

public class SttElementVisitor extends MetsElementVisitor {


    @Override
    protected boolean isMandatoryStream(String streamName) {
        return Const.sttMandatoryStreams.contains(streamName);
    }

}
