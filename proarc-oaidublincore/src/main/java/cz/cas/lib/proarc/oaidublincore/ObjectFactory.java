/*
 * Copyright (C) 2013 Jan Pokorsky
 *
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
package cz.cas.lib.proarc.oaidublincore;

import static cz.cas.lib.proarc.oaidublincore.DcConstants.*;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;

/**
 * The object factory to create JAXB elements of OAI DC schema.
 *
 * @author Jan Pokorsky
 */
@XmlRegistry
public final class ObjectFactory {

    public QName DC_QNAME = new QName(NS_OAIDC, DC);

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link OaiDcType }{@code >}}
     */
    @XmlElementDecl(name = DC, namespace = NS_OAIDC)
    public JAXBElement<OaiDcType> createDc(OaiDcType value) {
        return new JAXBElement<OaiDcType>(DC_QNAME, OaiDcType.class, null, value);
    }

}
