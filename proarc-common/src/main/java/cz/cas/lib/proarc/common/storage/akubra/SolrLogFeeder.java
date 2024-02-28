package cz.cas.lib.proarc.common.storage.akubra;

import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.incad.kramerius.resourceindex.ProcessingIndexFeeder;
import java.io.IOException;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.apache.solr.common.SolrInputDocument;

import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_DATE;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_OPERATION;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_PID;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_SOURCE;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_STREAM;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_USER;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.now;

public class SolrLogFeeder extends ProcessingIndexFeeder {

    public SolrLogFeeder(SolrClient solrClient) {
        super(solrClient);
    }

    public void feedLogChange(String user, String pid, String stream, String operation) throws DigitalObjectException {
        try {
            feedLogChange(user, now(), pid, stream, operation);
            commit();
        } catch (SolrServerException | IOException ex) {
            throw new DigitalObjectException(pid, "Nepodarilo se zaindexovat objekt " + pid + " do SOLRu.");
        }
    }

    private UpdateResponse feedLogChange(String user, String date, String pid, String stream, String operation) throws SolrServerException, IOException {
        SolrInputDocument sdoc = new SolrInputDocument();
        sdoc.addField(FIELD_SOURCE, getKey(pid, operation, stream, date));
        sdoc.addField(FIELD_USER, user);
        sdoc.addField(FIELD_DATE, date);
        sdoc.addField(FIELD_PID, pid);
        sdoc.addField(FIELD_STREAM, stream);
        sdoc.addField(FIELD_OPERATION, operation);
        return feedDescriptionDocument(sdoc);
    }

    private Object getKey(String pid, String operation, String stream, String date) {
        String key = pid + "-" + operation + "-" + stream + "-" + date;
        key = key.replaceAll(" ", "-").replaceAll(":", "-");
        return key;
    }

    public void feedIngestLog(String pid, String user) throws DigitalObjectException {
        feedLogChange(user, pid, null, "INGEST");
    }

    public void feedUpdateLog(String user, String pid, String dsId) throws DigitalObjectException {
        feedLogChange(user, pid, dsId, "UPDATE");
    }

    public void feedDeleteLog(String pid, String user) throws DigitalObjectException {
        feedLogChange(user, pid, null, "DEACTIVATE");
    }

    public void feedRestoreLog(String pid, String user) throws DigitalObjectException {
        feedLogChange(user, pid, null, "RESTORE");
    }

    public void feedPurgeLog(String pid, String user) throws DigitalObjectException {
        feedLogChange(user, pid, null, "PURGE");
    }
}
