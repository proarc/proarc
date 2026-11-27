package cz.cas.lib.proarc.common.process.external.bagit;

import java.io.IOException;
import java.nio.file.Path;

/** The source export required for packaging is no longer available. */
public final class MissingSourceDirectoryException extends IOException {
    public MissingSourceDirectoryException(Path directory) {
        super("Zdrojová složka " + directory.toAbsolutePath().normalize() + " již neexistuje.");
    }
}
