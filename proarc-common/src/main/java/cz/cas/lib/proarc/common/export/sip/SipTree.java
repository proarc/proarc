/*
 * Copyright (C) 2018 Martin Rumanek
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

package cz.cas.lib.proarc.common.export.sip;


import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;

public class SipTree {

    private final RemoteStorage remoteStorage;
    private final DigitalObject digitalObject;

    private SipTree(RemoteStorage remoteStorage, DigitalObject digitalObject) {
        this.remoteStorage = remoteStorage;
        this.digitalObject = digitalObject;
    }

    public static SipTree getTree(DigitalObject object, RemoteStorage remoteStorage) {
        return new SipTree(remoteStorage, object);
    }

    public List<SipTree> getChildren() {
        try {
            return Collections.unmodifiableList(remoteStorage.getSearch().findReferrers(digitalObject.getPID()).stream().map(item -> item.getPid())
                    .map(this::fetchTree).collect(Collectors.toList()));
        } catch (IOException e) {
            return Collections.emptyList();
        } catch (FedoraClientException e) {
            return Collections.emptyList();
        }
    }

    private Optional<SipTree> getParent() {
        try {
            return Optional.ofNullable(MetsUtils.getParent(digitalObject.getPID(), remoteStorage)).map(this::fetchTree);
        } catch (MetsExportException e) {
            return Optional.empty();
        }
    }

    public List<SipTree> getPathToRoot() {
        List<SipTree> path = new ArrayList<>();
        Optional<SipTree> parent = getParent();

        while (parent.isPresent()) {
            path.add(parent.get());
            parent = parent.get().getParent();
        }

        return Collections.unmodifiableList(path);
    }

    private SipTree fetchTree(String pid) {
        try {
            return new SipTree(remoteStorage, MetsUtils.readFoXML(pid, remoteStorage.getClient()));
        } catch (MetsExportException e1) {
            return null;
        }
    }


    public Stream<SipTree> flattened() {
        return Stream.concat(
                Stream.of(this),
                getChildren().stream().flatMap(SipTree::flattened));
    }

    @Override
    public String toString() {
        return "SipTree{" +
                "digitalObject=" + digitalObject +
                '}';
    }
}
