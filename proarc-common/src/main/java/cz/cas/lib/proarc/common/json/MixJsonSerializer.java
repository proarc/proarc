package cz.cas.lib.proarc.common.json;

import cz.cas.lib.proarc.mix.Mix;

public class MixJsonSerializer extends JaxbJsonSerializer<Mix> {

    public MixJsonSerializer() {
        super("ImageCaptureMetadata", JsonUtils.defaultObjectMapper());
    }
}
