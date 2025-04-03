/*
 * Copyright (C) 2025 Lukas Sykora
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
package cz.cas.lib.proarc.common.software;

/**
 * A software utils to generate default mets for each model and type of software
 *
 * @author Lukas Sykora
 */
public class SoftwareUtils {

    private static String TYPE_AGENT = "agent";

    private static String TYPE_EVENT_DIGITALIZATION = "eventDigitalization";
    private static String TYPE_EVENT_CREATION = "eventCreation";
    private static String TYPE_EVENT_XML_CREATION = "eventXmlCreation";
    private static String TYPE_EVENT_DELETION = "eventDeletion";

    private static String TYPE_OBJECT_PRESERVATION_ARCHIVAL = "objectPreservationArchival";
    private static String TYPE_OBJECT_PRESERVATION_ALTO = "objectPreservationAlto";
    private static String TYPE_OBJECT_DELETION_TIF = "objectDeletionTif";

    private static String DEFAULT_AGENT = "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?><mets:mets xmlns:premis=\"info:lc/xmlns/premis-v2\" xmlns:mets=\"http://www.loc.gov/METS/\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><mets:amdSec><mets:digiprovMD><mets:mdWrap MIMETYPE=\"text/xml\" MDTYPE=\"PREMIS\"><mets:xmlData><premis:agent><premis:agentIdentifier><premis:agentIdentifierType>ProArc_AgentID</premis:agentIdentifierType><premis:agentIdentifierValue>ProArc</premis:agentIdentifierValue></premis:agentIdentifier><premis:agentName>ProArc</premis:agentName><premis:agentType>software</premis:agentType></premis:agent></mets:xmlData></mets:mdWrap></mets:digiprovMD></mets:amdSec></mets:mets>";

    private static String DEFAULT_EVENT_DIGITALIZATION = "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?><mets:mets xmlns:premis=\"info:lc/xmlns/premis-v2\" xmlns:mets=\"http://www.loc.gov/METS/\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><mets:amdSec><mets:digiprovMD><mets:mdWrap MIMETYPE=\"text/xml\" MDTYPE=\"PREMIS\"><mets:xmlData><premis:event><premis:eventIdentifier><premis:eventIdentifierType>ProArc_EventID</premis:eventIdentifierType><premis:eventIdentifierValue>digitization_001</premis:eventIdentifierValue></premis:eventIdentifier><premis:eventType>capture</premis:eventType><premis:eventDateTime>${{date}}</premis:eventDateTime><premis:eventDetail>capture/digitization</premis:eventDetail><premis:eventOutcomeInformation><premis:eventOutcome>successful</premis:eventOutcome></premis:eventOutcomeInformation><premis:linkingObjectIdentifier><premis:linkingObjectIdentifierType>ProArc_URI</premis:linkingObjectIdentifierType><premis:linkingObjectIdentifierValue>${{uuid}}/RAW</premis:linkingObjectIdentifierValue></premis:linkingObjectIdentifier></premis:event></mets:xmlData></mets:mdWrap></mets:digiprovMD></mets:amdSec></mets:mets>";
    private static String DEFAULT_EVENT_CREATION = "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?><mets:mets xmlns:premis=\"info:lc/xmlns/premis-v2\" xmlns:mets=\"http://www.loc.gov/METS/\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><mets:amdSec><mets:digiprovMD><mets:mdWrap MIMETYPE=\"text/xml\" MDTYPE=\"PREMIS\"><mets:xmlData><premis:event><premis:eventIdentifier><premis:eventIdentifierType>ProArc_EventID</premis:eventIdentifierType><premis:eventIdentifierValue>MC_creation_001</premis:eventIdentifierValue></premis:eventIdentifier><premis:eventType>migration</premis:eventType><premis:eventDateTime>${{timestamp}}</premis:eventDateTime><premis:eventDetail>migration/MC_creation</premis:eventDetail><premis:eventOutcomeInformation><premis:eventOutcome>successful</premis:eventOutcome></premis:eventOutcomeInformation><premis:linkingObjectIdentifier><premis:linkingObjectIdentifierType>ProArc_URI</premis:linkingObjectIdentifierType><premis:linkingObjectIdentifierValue>${{uuid}}/NDK_ARCHIVAL</premis:linkingObjectIdentifierValue></premis:linkingObjectIdentifier></premis:event></mets:xmlData></mets:mdWrap></mets:digiprovMD></mets:amdSec></mets:mets>";
    private static String DEFAULT_EVENT_XML_CREATION = "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?><mets:mets xmlns:premis=\"info:lc/xmlns/premis-v2\" xmlns:mets=\"http://www.loc.gov/METS/\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><mets:amdSec><mets:digiprovMD><mets:mdWrap MIMETYPE=\"text/xml\" MDTYPE=\"PREMIS\"><mets:xmlData><premis:event><premis:eventIdentifier><premis:eventIdentifierType>ProArc_EventID</premis:eventIdentifierType><premis:eventIdentifierValue>XML_creation_001</premis:eventIdentifierValue></premis:eventIdentifier><premis:eventType>capture</premis:eventType><premis:eventDateTime>${{timestamp}}</premis:eventDateTime><premis:eventDetail>capture/XML_creation</premis:eventDetail><premis:eventOutcomeInformation><premis:eventOutcome>successful</premis:eventOutcome></premis:eventOutcomeInformation><premis:linkingObjectIdentifier><premis:linkingObjectIdentifierType>ProArc_URI</premis:linkingObjectIdentifierType><premis:linkingObjectIdentifierValue>${{uuid}}/ALTO</premis:linkingObjectIdentifierValue></premis:linkingObjectIdentifier></premis:event></mets:xmlData></mets:mdWrap></mets:digiprovMD></mets:amdSec></mets:mets>";
    private static String DEFAULT_EVENT_DELETION = "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?><mets:mets xmlns:premis=\"info:lc/xmlns/premis-v2\" xmlns:mets=\"http://www.loc.gov/METS/\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><mets:amdSec><mets:digiprovMD><mets:mdWrap MIMETYPE=\"text/xml\" MDTYPE=\"PREMIS\"><mets:xmlData><premis:event><premis:eventIdentifier><premis:eventIdentifierType>ProArc_EventID</premis:eventIdentifierType><premis:eventIdentifierValue>deletion_001</premis:eventIdentifierValue></premis:eventIdentifier><premis:eventType>deletion</premis:eventType><premis:eventDateTime>${{timestamp}}</premis:eventDateTime><premis:eventDetail>deletion/PS_deletion</premis:eventDetail><premis:eventOutcomeInformation><premis:eventOutcome>successful</premis:eventOutcome></premis:eventOutcomeInformation><premis:linkingObjectIdentifier><premis:linkingObjectIdentifierType>ProArc_URI</premis:linkingObjectIdentifierType><premis:linkingObjectIdentifierValue>${{uuid}}/RAW</premis:linkingObjectIdentifierValue></premis:linkingObjectIdentifier></premis:event></mets:xmlData></mets:mdWrap></mets:digiprovMD></mets:amdSec></mets:mets>";

    private static String DEFAULT_OBJECT_PRESERVATION_ARCHIVAL = "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?><mets:mets xmlns:premis=\"info:lc/xmlns/premis-v2\" xmlns:mets=\"http://www.loc.gov/METS/\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><mets:amdSec><mets:techMD><mets:mdWrap MIMETYPE=\"text/xml\" MDTYPE=\"PREMIS\"><mets:xmlData><premis:object xsi:type=\"premis:file\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><premis:objectIdentifier><premis:objectIdentifierType>ProArc_URI</premis:objectIdentifierType><premis:objectIdentifierValue>${{uuid}}/NDK_ARCHIVAL</premis:objectIdentifierValue></premis:objectIdentifier><premis:preservationLevel><premis:preservationLevelValue>preservation</premis:preservationLevelValue></premis:preservationLevel><premis:objectCharacteristics><premis:compositionLevel>0</premis:compositionLevel><premis:fixity><premis:messageDigestAlgorithm>MD5</premis:messageDigestAlgorithm><premis:messageDigestOriginator>ProArc</premis:messageDigestOriginator></premis:fixity><premis:size>${{sizeNdkArchival}}</premis:size><premis:format><premis:formatDesignation><premis:formatName>image/jp2</premis:formatName><premis:formatVersion>1.0</premis:formatVersion></premis:formatDesignation><premis:formatRegistry><premis:formatRegistryName>PRONOM</premis:formatRegistryName><premis:formatRegistryKey>x-fmt/392</premis:formatRegistryKey></premis:formatRegistry></premis:format><premis:creatingApplication><premis:creatingApplicationName>ProArc</premis:creatingApplicationName><premis:creatingApplicationVersion>${{proarcVersion}}</premis:creatingApplicationVersion><premis:dateCreatedByApplication>2${{timestamp}}</premis:dateCreatedByApplication></premis:creatingApplication></premis:objectCharacteristics><premis:originalName>${{fileNameNdkArchival}}</premis:originalName><premis:relationship><premis:relationshipType>derivation</premis:relationshipType><premis:relationshipSubType>created from</premis:relationshipSubType><premis:relatedObjectIdentification><premis:relatedObjectIdentifierType>ProArc_URI</premis:relatedObjectIdentifierType><premis:relatedObjectIdentifierValue>${{uuid}}/RAW</premis:relatedObjectIdentifierValue></premis:relatedObjectIdentification></premis:relationship></premis:object></mets:xmlData></mets:mdWrap></mets:techMD></mets:amdSec></mets:mets>";
    private static String DEFAULT_OBJECT_PRESERVATION_ALTO = "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?><mets:mets xmlns:premis=\"info:lc/xmlns/premis-v2\" xmlns:mets=\"http://www.loc.gov/METS/\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><mets:amdSec><mets:techMD><mets:mdWrap MIMETYPE=\"text/xml\" MDTYPE=\"PREMIS\"><mets:xmlData><premis:object xsi:type=\"premis:file\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><premis:objectIdentifier><premis:objectIdentifierType>ProArc_URI</premis:objectIdentifierType><premis:objectIdentifierValue>${{uuid}}/ALTO</premis:objectIdentifierValue></premis:objectIdentifier><premis:preservationLevel><premis:preservationLevelValue>preservation</premis:preservationLevelValue></premis:preservationLevel><premis:objectCharacteristics><premis:compositionLevel>0</premis:compositionLevel><premis:fixity><premis:messageDigestAlgorithm>MD5</premis:messageDigestAlgorithm><premis:messageDigestOriginator>ProArc</premis:messageDigestOriginator></premis:fixity><premis:size>${{sizeAlto}}</premis:size><premis:format><premis:formatDesignation><premis:formatName>text/xml</premis:formatName><premis:formatVersion>1.0</premis:formatVersion></premis:formatDesignation><premis:formatRegistry><premis:formatRegistryName>PRONOM</premis:formatRegistryName><premis:formatRegistryKey>fmt/101</premis:formatRegistryKey></premis:formatRegistry></premis:format><premis:creatingApplication><premis:creatingApplicationName>ProArc</premis:creatingApplicationName><premis:creatingApplicationVersion>${{proarcVersion}}</premis:creatingApplicationVersion><premis:dateCreatedByApplication>${{timestamp}}</premis:dateCreatedByApplication></premis:creatingApplication></premis:objectCharacteristics><premis:originalName>${{filenameAlto}}</premis:originalName><premis:relationship><premis:relationshipType>derivation</premis:relationshipType><premis:relationshipSubType>created from</premis:relationshipSubType><premis:relatedObjectIdentification><premis:relatedObjectIdentifierType>ProArc_URI</premis:relatedObjectIdentifierType><premis:relatedObjectIdentifierValue>${{uuid}}/NDK_ARCHIVAL</premis:relatedObjectIdentifierValue></premis:relatedObjectIdentification></premis:relationship></premis:object></mets:xmlData></mets:mdWrap></mets:techMD></mets:amdSec></mets:mets>";
    private static String DEFAULT_OBJECT_DELETION_TIF = "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?><mets:mets xmlns:premis=\"info:lc/xmlns/premis-v2\" xmlns:mets=\"http://www.loc.gov/METS/\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><mets:amdSec><mets:techMD><mets:mdWrap MIMETYPE=\"text/xml\" MDTYPE=\"PREMIS\"><mets:xmlData><premis:object xsi:type=\"premis:file\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><premis:objectIdentifier><premis:objectIdentifierType>ProArc_URI</premis:objectIdentifierType><premis:objectIdentifierValue>${{uuid}}/RAW</premis:objectIdentifierValue></premis:objectIdentifier><premis:preservationLevel><premis:preservationLevelValue>deleted</premis:preservationLevelValue></premis:preservationLevel><premis:objectCharacteristics><premis:compositionLevel>0</premis:compositionLevel><premis:fixity><premis:messageDigestAlgorithm>MD5</premis:messageDigestAlgorithm><premis:messageDigestOriginator>ProArc</premis:messageDigestOriginator></premis:fixity><premis:size>${{sizeTif}}</premis:size><premis:format><premis:formatDesignation><premis:formatName>image/tiff</premis:formatName><premis:formatVersion>1.0</premis:formatVersion></premis:formatDesignation><premis:formatRegistry><premis:formatRegistryName>PRONOM</premis:formatRegistryName><premis:formatRegistryKey>fmt/353</premis:formatRegistryKey></premis:formatRegistry></premis:format><premis:creatingApplication><premis:creatingApplicationName>ProArc</premis:creatingApplicationName><premis:creatingApplicationVersion>${{proarcVersion}}</premis:creatingApplicationVersion><premis:dateCreatedByApplication>${{filenameTif}}</premis:dateCreatedByApplication></premis:creatingApplication></premis:objectCharacteristics><premis:originalName>${{filenameTif}}</premis:originalName></premis:object></mets:xmlData></mets:mdWrap></mets:techMD></mets:amdSec></mets:mets>";


    public static String createDefaultDescription(String model, String type) throws SoftwareException {
        if (SoftwareRepository.METAMODEL_AGENT_ID.equals(model)) {
            return DEFAULT_AGENT;
        } else if(SoftwareRepository.METAMODEL_EVENT_ID.equals(model)) {
            if (TYPE_EVENT_CREATION.equals(type)) {
                return DEFAULT_EVENT_CREATION;
            } else if (TYPE_EVENT_XML_CREATION.equals(type)) {
                return DEFAULT_EVENT_XML_CREATION;
            } else if (TYPE_EVENT_DELETION.equals(type)) {
                return DEFAULT_EVENT_DELETION;
            } else if (TYPE_EVENT_DIGITALIZATION.equals(type)) {
                return DEFAULT_EVENT_DIGITALIZATION;
            } else {
                throw new SoftwareException("Unknown type (\"" + type + "\") for creating default premis for " + model);
            }
        } else if (SoftwareRepository.METAMODEL_OBJECT_ID.equals(model)) {
            if (TYPE_OBJECT_DELETION_TIF.equals(type)) {
                return DEFAULT_OBJECT_DELETION_TIF;
            } else if (TYPE_OBJECT_PRESERVATION_ALTO.equals(type)) {
                return DEFAULT_OBJECT_PRESERVATION_ALTO;
            } else if (TYPE_OBJECT_PRESERVATION_ARCHIVAL.equals(type)) {
                return DEFAULT_OBJECT_PRESERVATION_ARCHIVAL;
            } else {
                throw new SoftwareException("Unknown type (\"" + type + "\") for creating default premis for " + model);
            }
        } else {
            throw new SoftwareException("Unknown model (\"" + model + "\") for creating default premis.");
        }
    }
}
