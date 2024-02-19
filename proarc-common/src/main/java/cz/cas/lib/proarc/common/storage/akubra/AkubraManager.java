package cz.cas.lib.proarc.common.storage.akubra;

import com.yourmediashelf.fedora.generated.foxml.ContentLocationType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.foxml.PropertyType;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.net.URI;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.datatype.XMLGregorianCalendar;
import org.akubraproject.BlobStore;
import org.akubraproject.fs.FSBlobStore;
import org.akubraproject.map.IdMapper;
import org.akubraproject.map.IdMappingBlobStore;
import org.fcrepo.server.errors.LowlevelStorageException;
import org.fcrepo.server.errors.ObjectAlreadyInLowlevelStorageException;
import org.fcrepo.server.errors.ObjectNotInLowlevelStorageException;
import org.fcrepo.server.storage.lowlevel.ICheckable;
import org.fcrepo.server.storage.lowlevel.ILowlevelStorage;
import org.fcrepo.server.storage.lowlevel.akubra.AkubraLowlevelStorage;
import org.fcrepo.server.storage.lowlevel.akubra.HashPathIdMapper;

public class AkubraManager {

    public static final Logger LOGGER = Logger.getLogger(AkubraManager.class.getName());
    private ILowlevelStorage storage;

    private static Unmarshaller unmarshaller = null;
    private static Marshaller marshaller = null;

    static {
        try {
            JAXBContext jaxbContext = JAXBContext.newInstance(DigitalObject.class);
            unmarshaller = jaxbContext.createUnmarshaller();

            //JAXBContext jaxbdatastreamContext = JAXBContext.newInstance(DatastreamType.class);
            marshaller = jaxbContext.createMarshaller();

        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Cannot init JAXB", e);
            throw new RuntimeException(e);
        }
    }

    public AkubraManager(AkubraConfiguration configuration) throws IOException {
        try {
            this.storage = createAkubraLowLevelStorage(configuration);
        } catch (Exception ex) {
            throw new IOException(ex);
        }
    }

    private AkubraLowlevelStorage createAkubraLowLevelStorage(AkubraConfiguration configuration) throws Exception {
        BlobStore fsObjectStore = new FSBlobStore(new URI("urn:example.org:fsObjectStore"), new File(configuration.getObjectStorePath()));
        IdMapper fsObjectStoreMapper = new HashPathIdMapper(configuration.getObjectStorePattern());
        BlobStore objectStore = new IdMappingBlobStore(new URI("urn:example.org:objectStore"), fsObjectStore, fsObjectStoreMapper);
        BlobStore fsDatastreamStore = new FSBlobStore(new URI("urn:example.org:fsDatastreamStore"), new File(configuration.getDatastreamStorePath()));
        IdMapper fsDatastreamStoreMapper = new HashPathIdMapper(configuration.getDatastreamStorePattern());
        BlobStore datastreamStore = new IdMappingBlobStore(new URI("urn:example.org:datastreamStore"), fsDatastreamStore, fsDatastreamStoreMapper);
        AkubraLowlevelStorage retval = new AkubraLowlevelStorage(objectStore, datastreamStore, true, true);
        return retval;
    }

    public boolean objectExists(String pid) throws DigitalObjectException {
        return readObjectFromStorage(pid) != null;
    }

    public DigitalObject readObjectFromStorage(String pid) throws DigitalObjectException {
        Object obj;
        try (InputStream inputStream = this.storage.retrieveObject(pid);){
            synchronized (unmarshaller) {
                obj = unmarshaller.unmarshal(inputStream);
            }
        } catch (ObjectNotInLowlevelStorageException ex) {
            return null;
        } catch (Exception e) {
            throw new DigitalObjectException(pid, e);
        }
        return (DigitalObject) obj;
    }

    public InputStream retrieveDatastream(String dsKey) throws IOException {
        try {
            return storage.retrieveDatastream(dsKey);
        } catch (LowlevelStorageException e) {
            throw new IOException(e);
        }
    }

    public InputStream retrieveObject(String objectKey) throws IOException {
        try {
            return storage.retrieveObject(objectKey);
        } catch (LowlevelStorageException e) {
            throw new IOException(e);
        }
    }

    public void deleteObject(String pid, boolean includingManagedDatastreams) throws IOException, DigitalObjectException {
        DigitalObject object = readObjectFromStorage(pid);
        if (includingManagedDatastreams) {
            for (DatastreamType datastreamType : object.getDatastream()) {
                removeManagedStream(datastreamType);
            }
        }
        try {
            storage.removeObject(pid);
        } catch (LowlevelStorageException e) {
            LOGGER.warning("Could not remove object from Akubra: " + e);
        }
    }

    public void deleteStream(String pid, String streamId) throws IOException, DigitalObjectException {
        DigitalObject object = readObjectFromStorage(pid);
        List<DatastreamType> datastreamList = object.getDatastream();
        Iterator<DatastreamType> iterator = datastreamList.iterator();
        while (iterator.hasNext()) {
            DatastreamType datastreamType = iterator.next();
            if (streamId.equals(datastreamType.getID())) {
                removeManagedStream(datastreamType);
                iterator.remove();
                break;
            }
        }
        try {
            setLastModified(object);
            StringWriter stringWriter = new StringWriter();
            synchronized (marshaller) {
                marshaller.marshal(object, stringWriter);
            }
            addOrReplaceObject(pid, new ByteArrayInputStream(stringWriter.toString().getBytes("UTF-8")));
        } catch (Exception e) {
            LOGGER.warning("Could not replace object in Akubra: " + e);
        }
    }

    private void removeManagedStream(DatastreamType datastreamType) {
        if ("M".equals(datastreamType.getCONTROLGROUP())) {
            for (DatastreamVersionType datastreamVersionType : datastreamType.getDatastreamVersion()) {
                if ("INTERNAL_ID".equals(datastreamVersionType.getContentLocation().getTYPE())) {
                    try {
                        storage.removeDatastream(datastreamVersionType.getContentLocation().getREF());
                    } catch (LowlevelStorageException e) {
                        LOGGER.warning("Could not remove managed datastream from Akubra: " + e);
                    }
                }
            }
        }
    }

    public void commit(DigitalObject object, String streamId) throws IOException {
        List<DatastreamType> datastreamList = object.getDatastream();
        Iterator<DatastreamType> iterator = datastreamList.iterator();
        while (iterator.hasNext()) {
            DatastreamType datastream = iterator.next();
            ensureDsVersionCreatedDate(datastream);
            if (streamId != null && streamId.equals(datastream.getID())) {
                convertManagedStream(object.getPID(), datastream);
                break;
            } else {
                convertManagedStream(object.getPID(), datastream);
            }
        }
        try {
            setLastModified(object);
            ensureCreatedDate(object);
            ensureActive(object);
            StringWriter stringWriter = new StringWriter();
            synchronized (marshaller) {
                marshaller.marshal(object, stringWriter);
            }
            addOrReplaceObject(object.getPID(), new ByteArrayInputStream(stringWriter.toString().getBytes("UTF-8")));
        } catch (Exception e) {
            LOGGER.warning("Could not replace object in Akubra: " + e);
        }
    }

    public InputStream marshallObject(DigitalObject object) {
        try {
            StringWriter stringWriter = new StringWriter();
            synchronized (marshaller) {
                marshaller.marshal(object, stringWriter);
            }
            return new ByteArrayInputStream(stringWriter.toString().getBytes("UTF-8"));
        } catch (Exception e) {
            LOGGER.warning("Could not marshall object: " + e);
            throw new RuntimeException(e);
        }
    }

    private void setLastModified(DigitalObject object) {
        boolean propertyExists = false;
        List<PropertyType> propertyTypeList = object.getObjectProperties().getProperty();
        for (PropertyType propertyType : propertyTypeList) {
            if ("info:fedora/fedora-system:def/view#lastModifiedDate".equals(propertyType.getNAME())) {
                propertyType.setVALUE(AkubraUtils.currentTimeString());
                propertyExists = true;
                break;
            }
        }
        if (!propertyExists) {
            propertyTypeList.add(AkubraUtils.createProperty("info:fedora/fedora-system:def/view#lastModifiedDate", AkubraUtils.currentTimeString()));
        }
    }

    private void ensureCreatedDate(DigitalObject object) {
        boolean propertyExists = false;
        List<PropertyType> propertyTypeList = object.getObjectProperties().getProperty();
        for (PropertyType propertyType : propertyTypeList) {
            if ("info:fedora/fedora-system:def/model#createdDate".equals(propertyType.getNAME())) {
                propertyExists = true;
                break;
            }
        }
        if (!propertyExists) {
            propertyTypeList.add(AkubraUtils.createProperty("info:fedora/fedora-system:def/model#createdDate", AkubraUtils.currentTimeString()));
        }
    }

    private void ensureActive(DigitalObject object) {
        boolean propertyExists = false;
        List<PropertyType> propertyTypeList = object.getObjectProperties().getProperty();
        for (PropertyType propertyType : propertyTypeList) {
            if ("info:fedora/fedora-system:def/model#state".equals(propertyType.getNAME())) {
                propertyExists = true;
                break;
            }
        }
        if (!propertyExists) {
            propertyTypeList.add(AkubraUtils.createProperty("info:fedora/fedora-system:def/model#state", "Active"));
        }
    }

    private void ensureDsVersionCreatedDate(DatastreamType datastream) {
        if (datastream != null) {
            for (DatastreamVersionType datastreamVersion : datastream.getDatastreamVersion()) {
                XMLGregorianCalendar created = datastreamVersion.getCREATED();
                if (created == null) {
                    datastreamVersion.setCREATED(AkubraUtils.getCurrentXMLGregorianCalendar());
                }
            }
        }
    }

    private void convertManagedStream(String pid, DatastreamType datastream) {
        if ("M".equals(datastream.getCONTROLGROUP())) {
            for (DatastreamVersionType datastreamVersion : datastream.getDatastreamVersion()) {
                if (datastreamVersion.getBinaryContent() != null) {
                    try {
                        String ref = pid + "+" + datastream.getID() + "+" + datastreamVersion.getID();
                        addOrReplaceDatastream(ref, new ByteArrayInputStream(datastreamVersion.getBinaryContent()));
                        datastreamVersion.setBinaryContent(null);
                        ContentLocationType contentLocationType = new ContentLocationType();
                        contentLocationType.setTYPE("INTERNAL_ID");
                        contentLocationType.setREF(ref);
                        datastreamVersion.setContentLocation(contentLocationType);
                    } catch (LowlevelStorageException e) {
                        LOGGER.warning("Could not remove managed datastream from Akubra: " + e);
                    }
                }
            }
        }
    }

    public void addOrReplaceObject(String pid, InputStream content) throws LowlevelStorageException {
        if (((ICheckable) storage).objectExists(pid)) {
            storage.replaceObject(pid, content, null);
        } else {
            storage.addObject(pid, content, null);
        }
    }

    public void addOrReplaceDatastream(String pid, InputStream content) throws LowlevelStorageException {
        if (storage instanceof AkubraLowlevelStorage) {
            if (((AkubraLowlevelStorage) storage).datastreamExists(pid)) {
                storage.replaceDatastream(pid, content, null);
            } else {
                storage.addDatastream(pid, content, null);
            }
        } else {
            try {
                storage.addDatastream(pid, content, null);
            } catch (ObjectAlreadyInLowlevelStorageException oailse) {
                storage.replaceDatastream(pid, content, null);
            }
        }
    }

}
