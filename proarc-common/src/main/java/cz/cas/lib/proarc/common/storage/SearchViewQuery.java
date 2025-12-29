package cz.cas.lib.proarc.common.storage;

import java.util.Collection;
import java.util.Collections;

public class SearchViewQuery {

    private String title;
    private String processor;
    private String label;
    private String identifier;
    private String owner;
    private String model;
    private String status;
    private String parentPid;
    private Collection<String> hasOwners;

    public String getOrganization() {
        return organization;
    }

    public SearchViewQuery setOrganization(String organization) {
        this.organization = organization;
        return this;
    }

    private String organization;

    public String getTitle() {
        return title;
    }

    public SearchViewQuery setTitle(String title) {
        this.title = title;
        return this;
    }

    public String getProcessor() {
        return processor;
    }

    public SearchViewQuery setProcessor(String processor) {
        this.processor = processor;
        return this;
    }

    public String getLabel() {
        return label;
    }

    public SearchViewQuery setLabel(String label) {
        this.label = label;
        return this;
    }

    public String getIdentifier() {
        return identifier;
    }

    public SearchViewQuery setIdentifier(String identifier) {
        this.identifier = identifier;
        return this;
    }

    public String getOwner() {
        return owner;
    }

    public SearchViewQuery setOwner(String owner) {
        this.owner = owner;
        return this;
    }

    public String getModel() {
        return model;
    }

    public SearchViewQuery setModel(String model) {
        this.model = model;
        return this;
    }

    public Collection<String> getHasOwners() {
        return hasOwners != null ? hasOwners : Collections.<String>emptyList();
    }

    public SearchViewQuery setHasOwners(Collection<String> hasOwners) {
        this.hasOwners = hasOwners;
        return this;
    }

    public String getStatus() {
        return status;
    }

    public SearchViewQuery setStatus(String status) {
        this.status = status;
        return this;
    }


    public String getParentPid() {
        return parentPid;
    }

    public SearchViewQuery setParentPid(String parentPid) {
        this.parentPid = parentPid;
        return this;
    }
}
