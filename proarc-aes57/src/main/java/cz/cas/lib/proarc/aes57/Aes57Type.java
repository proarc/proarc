/*
 * Copyright (C) 2017 Lukas Sykora
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
package cz.cas.lib.proarc.aes57;

import org.aes.audioobject.*;
import javax.xml.bind.annotation.*;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Aes57Type", namespace ="http://www.aes.org/audioObject", propOrder = {
        "audioObjectType",
        "physicalPropertiesType",
        "physicalStructureType",
        "tapeStructureType",
        "opticalStructureType",
        "analogDiscStructureType",
        "cylinderStructureType",
        "baseDimensionsType",
        "tapeDimensionsType",
        "wireDimensionsType",
        "analogDiscDimensionsType",
        "opticalDiscDimensionsType",
        "cylinderDimensionsType",
        "shellDimensionsType",
        "dimensionsType",
        "faceType",
        "faceRegionType",
        "streamType",
        "formatRegionListType",
        "baseFormatRegionType",
        "formatRegionType",
        "analogTapeFormatRegionType",
        "digitalTapeFormatRegionType",
        "analogDiscFormatRegionType",
        "opticalDiscFormatRegionType",
        "wireFormatRegionType",
        "cylinderFormatRegionType",
        "formatType",
        "layerType",
        "roleType",
        "measurementType",
        "measurementUnitsType",
        "appSpecificDataType",
        "byteOrderType",
        "useType",
        "useTypeType",
        "identifierType",
        "identifierTypeType",
        "checksumType",
        "checksumKindType"
})

@XmlSeeAlso({
        Aes57.class
})
public class Aes57Type {

    @XmlElement(name = "audioObjectType", namespace = "http://www.aes.org/audioObject")
    protected AudioObjectType audioObjectType;
    @XmlElement(name = "physicalPropertiesType", namespace = "http://www.aes.org/audioObject")
    protected PhysicalPropertiesType physicalPropertiesType;
    @XmlElement(name = "physicalStructureType", namespace = "http://www.aes.org/audioObject")
    protected PhysicalStructureType physicalStructureType;
    @XmlElement(name = "tapeStructureType", namespace = "http://www.aes.org/audioObject")
    protected TapeStructureType tapeStructureType;
    @XmlElement(name = "opticalStructureType", namespace = "http://www.aes.org/audioObject")
    protected OpticalStructureType opticalStructureType;
    @XmlElement(name = "analogDiscStructureType", namespace = "http://www.aes.org/audioObject")
    protected AnalogDiscStructureType analogDiscStructureType;
    @XmlElement(name = "cylinderStructureType", namespace = "http://www.aes.org/audioObject")
    protected CylinderStructureType cylinderStructureType;
    @XmlElement(name = "baseDimensionsType", namespace = "http://www.aes.org/audioObject")
    protected BaseDimensionsType baseDimensionsType;
    @XmlElement(name = "tapeDimensionsType", namespace = "http://www.aes.org/audioObject")
    protected TapeDimensionsType tapeDimensionsType;
    @XmlElement(name = "wireDimensionsType",namespace = "http://www.aes.org/audioObject")
    protected WireDimensionsType wireDimensionsType;
    @XmlElement(name = "analogDiscDimensionsType", namespace = "http://www.aes.org/audioObject")
    protected AnalogDiscDimensionsType analogDiscDimensionsType;
    @XmlElement(name = "opticalDiscDimensionsType", namespace = "http://www.aes.org/audioObject")
    protected OpticalDiscDimensionsType opticalDiscDimensionsType;
    @XmlElement(name = "cylinderDimensionsType", namespace = "http://www.aes.org/audioObject")
    protected CylinderDimensionsType cylinderDimensionsType;
    @XmlElement(name = "shellDimensionsType", namespace = "http://www.aes.org/audioObject")
    protected ShellDimensionsType shellDimensionsType;
    @XmlElement(name = "dimensionsType", namespace = "http://www.aes.org/audioObject")
    protected DimensionsType dimensionsType;
    @XmlElement(name = "faceType", namespace = "http://www.aes.org/audioObject")
    protected FaceType faceType;
    @XmlElement(name = "faceRegionType", namespace = "http://www.aes.org/audioObject")
    protected FaceRegionType faceRegionType;
    @XmlElement(name = "streamType", namespace = "http://www.aes.org/audioObject")
    protected StreamType streamType;
    @XmlElement(name = "formatRegionListType", namespace = "http://www.aes.org/audioObject")
    protected FormatRegionListType formatRegionListType;
    @XmlElement(name = "baseFormatRegionType", namespace = "http://www.aes.org/audioObject")
    protected BaseFormatRegionType baseFormatRegionType;
    @XmlElement(name = "formatRegionType", namespace = "http://www.aes.org/audioObject")
    protected FormatRegionType formatRegionType;
    @XmlElement(name = "analogTapeFormatRegionType", namespace = "http://www.aes.org/audioObject")
    protected AnalogTapeFormatRegionType analogTapeFormatRegionType;
    @XmlElement(name = "digitalTapeFormatRegionType", namespace = "http://www.aes.org/audioObject")
    protected DigitalTapeFormatRegionType digitalTapeFormatRegionType;
    @XmlElement(name = "analogDiscFormatRegionType", namespace = "http://www.aes.org/audioObject")
    protected AnalogDiscFormatRegionType analogDiscFormatRegionType;
    @XmlElement(name = "opticalDiscFormatRegionType", namespace = "http://www.aes.org/audioObject")
    protected OpticalDiscFormatRegionType opticalDiscFormatRegionType;
    @XmlElement(name = "wireFormatRegionType", namespace = "http://www.aes.org/audioObject")
    protected WireFormatRegionType wireFormatRegionType;
    @XmlElement(name = "cylinderFormatRegionType", namespace = "http://www.aes.org/audioObject")
    protected CylinderFormatRegionType cylinderFormatRegionType;
    @XmlElement(name = "formatType", namespace = "http://www.aes.org/audioObject")
    protected FormatType formatType;
    @XmlElement(name = "layerType", namespace = "http://www.aes.org/audioObject")
    protected LayerType layerType;
    @XmlElement(name = "roleType", namespace = "http://www.aes.org/audioObject")
    protected RoleType roleType;
    @XmlElement(name = "measurementType", namespace = "http://www.aes.org/audioObject")
    protected MeasurementType measurementType;
    @XmlElement(name = "measurementUnitsType", namespace = "http://www.aes.org/audioObject")
    protected MeasurementUnitsType measurementUnitsType;
    @XmlElement(name = "appSpecificDataType", namespace = "http://www.aes.org/audioObject")
    protected AppSpecificDataType appSpecificDataType;
    @XmlElement(name = "byteOrderType", namespace = "http://www.aes.org/audioObject")
    protected ByteOrderType byteOrderType;
    @XmlElement(name = "useType", namespace = "http://www.aes.org/audioObject")
    protected UseType useType;
    @XmlElement(name = "useTypeType", namespace = "http://www.aes.org/audioObject")
    protected UseTypeType useTypeType;
    @XmlElement(name = "identifierType", namespace = "http://www.aes.org/audioObject")
    protected IdentifierType identifierType;
    @XmlElement(name = "identifierTypeType", namespace = "http://www.aes.org/audioObject")
    protected IdentifierTypeType identifierTypeType;
    @XmlElement(name = "checksumType", namespace = "http://www.aes.org/audioObject")
    protected ChecksumType checksumType;
    @XmlElement(name = "checksumKindType", namespace = "http://www.aes.org/audioObject")
    protected ChecksumKindType checksumKindType;

    public AudioObjectType getAudioObjectType() {
        return audioObjectType;
    }

    public void setAudioObjectType(AudioObjectType audioObjectType) {
        this.audioObjectType = audioObjectType;
    }

    public PhysicalPropertiesType getPhysicalPropertiesType() {
        return physicalPropertiesType;
    }

    public void setPhysicalPropertiesType(PhysicalPropertiesType physicalPropertiesType) {
        this.physicalPropertiesType = physicalPropertiesType;
    }

    public PhysicalStructureType getPhysicalStructureType() {
        return physicalStructureType;
    }

    public void setPhysicalStructureType(PhysicalStructureType physicalStructureType) {
        this.physicalStructureType = physicalStructureType;
    }

    public TapeStructureType getTapeStructureType() {
        return tapeStructureType;
    }

    public void setTapeStructureType(TapeStructureType tapeStructureType) {
        this.tapeStructureType = tapeStructureType;
    }

    public OpticalStructureType getOpticalStructureType() {
        return opticalStructureType;
    }

    public void setOpticalStructureType(OpticalStructureType opticalStructureType) {
        this.opticalStructureType = opticalStructureType;
    }

    public AnalogDiscStructureType getAnalogDiscStructureType() {
        return analogDiscStructureType;
    }

    public void setAnalogDiscStructureType(AnalogDiscStructureType analogDiscStructureType) {
        this.analogDiscStructureType = analogDiscStructureType;
    }

    public CylinderStructureType getCylinderStructureType() {
        return cylinderStructureType;
    }

    public void setCylinderStructureType(CylinderStructureType cylinderStructureType) {
        this.cylinderStructureType = cylinderStructureType;
    }

    public BaseDimensionsType getBaseDimensionsType() {
        return baseDimensionsType;
    }

    public void setBaseDimensionsType(BaseDimensionsType baseDimensionsType) {
        this.baseDimensionsType = baseDimensionsType;
    }

    public TapeDimensionsType getTapeDimensionsType() {
        return tapeDimensionsType;
    }

    public void setTapeDimensionsType(TapeDimensionsType tapeDimensionsType) {
        this.tapeDimensionsType = tapeDimensionsType;
    }

    public WireDimensionsType getWireDimensionsType() {
        return wireDimensionsType;
    }

    public void setWireDimensionsType(WireDimensionsType wireDimensionsType) {
        this.wireDimensionsType = wireDimensionsType;
    }

    public AnalogDiscDimensionsType getAnalogDiscDimensionsType() {
        return analogDiscDimensionsType;
    }

    public void setAnalogDiscDimensionsType(AnalogDiscDimensionsType analogDiscDimensionsType) {
        this.analogDiscDimensionsType = analogDiscDimensionsType;
    }

    public OpticalDiscDimensionsType getOpticalDiscDimensionsType() {
        return opticalDiscDimensionsType;
    }

    public void setOpticalDiscDimensionsType(OpticalDiscDimensionsType opticalDiscDimensionsType) {
        this.opticalDiscDimensionsType = opticalDiscDimensionsType;
    }

    public CylinderDimensionsType getCylinderDimensionsType() {
        return cylinderDimensionsType;
    }

    public void setCylinderDimensionsType(CylinderDimensionsType cylinderDimensionsType) {
        this.cylinderDimensionsType = cylinderDimensionsType;
    }

    public ShellDimensionsType getShellDimensionsType() {
        return shellDimensionsType;
    }

    public void setShellDimensionsType(ShellDimensionsType shellDimensionsType) {
        this.shellDimensionsType = shellDimensionsType;
    }

    public DimensionsType getDimensionsType() {
        return dimensionsType;
    }

    public void setDimensionsType(DimensionsType dimensionsType) {
        this.dimensionsType = dimensionsType;
    }

    public FaceType getFaceType() {
        return faceType;
    }

    public void setFaceType(FaceType faceType) {
        this.faceType = faceType;
    }

    public FaceRegionType getFaceRegionType() {
        return faceRegionType;
    }

    public void setFaceRegionType(FaceRegionType faceRegionType) {
        this.faceRegionType = faceRegionType;
    }

    public StreamType getStreamType() {
        return streamType;
    }

    public void setStreamType(StreamType streamType) {
        this.streamType = streamType;
    }

    public FormatRegionListType getFormatRegionListType() {
        return formatRegionListType;
    }

    public void setFormatRegionListType(FormatRegionListType formatRegionListType) {
        this.formatRegionListType = formatRegionListType;
    }

    public BaseFormatRegionType getBaseFormatRegionType() {
        return baseFormatRegionType;
    }

    public void setBaseFormatRegionType(BaseFormatRegionType baseFormatRegionType) {
        this.baseFormatRegionType = baseFormatRegionType;
    }

    public FormatRegionType getFormatRegionType() {
        return formatRegionType;
    }

    public void setFormatRegionType(FormatRegionType formatRegionType) {
        this.formatRegionType = formatRegionType;
    }

    public AnalogTapeFormatRegionType getAnalogTapeFormatRegionType() {
        return analogTapeFormatRegionType;
    }

    public void setAnalogTapeFormatRegionType(AnalogTapeFormatRegionType analogTapeFormatRegionType) {
        this.analogTapeFormatRegionType = analogTapeFormatRegionType;
    }

    public DigitalTapeFormatRegionType getDigitalTapeFormatRegionType() {
        return digitalTapeFormatRegionType;
    }

    public void setDigitalTapeFormatRegionType(DigitalTapeFormatRegionType digitalTapeFormatRegionType) {
        this.digitalTapeFormatRegionType = digitalTapeFormatRegionType;
    }

    public AnalogDiscFormatRegionType getAnalogDiscFormatRegionType() {
        return analogDiscFormatRegionType;
    }

    public void setAnalogDiscFormatRegionType(AnalogDiscFormatRegionType analogDiscFormatRegionType) {
        this.analogDiscFormatRegionType = analogDiscFormatRegionType;
    }

    public OpticalDiscFormatRegionType getOpticalDiscFormatRegionType() {
        return opticalDiscFormatRegionType;
    }

    public void setOpticalDiscFormatRegionType(OpticalDiscFormatRegionType opticalDiscFormatRegionType) {
        this.opticalDiscFormatRegionType = opticalDiscFormatRegionType;
    }

    public WireFormatRegionType getWireFormatRegionType() {
        return wireFormatRegionType;
    }

    public void setWireFormatRegionType(WireFormatRegionType wireFormatRegionType) {
        this.wireFormatRegionType = wireFormatRegionType;
    }

    public CylinderFormatRegionType getCylinderFormatRegionType() {
        return cylinderFormatRegionType;
    }

    public void setCylinderFormatRegionType(CylinderFormatRegionType cylinderFormatRegionType) {
        this.cylinderFormatRegionType = cylinderFormatRegionType;
    }

    public FormatType getFormatType() {
        return formatType;
    }

    public void setFormatType(FormatType formatType) {
        this.formatType = formatType;
    }

    public LayerType getLayerType() {
        return layerType;
    }

    public void setLayerType(LayerType layerType) {
        this.layerType = layerType;
    }

    public RoleType getRoleType() {
        return roleType;
    }

    public void setRoleType(RoleType roleType) {
        this.roleType = roleType;
    }

    public MeasurementType getMeasurementType() {
        return measurementType;
    }

    public void setMeasurementType(MeasurementType measurementType) {
        this.measurementType = measurementType;
    }

    public MeasurementUnitsType getMeasurementUnitsType() {
        return measurementUnitsType;
    }

    public void setMeasurementUnitsType(MeasurementUnitsType measurementUnitsType) {
        this.measurementUnitsType = measurementUnitsType;
    }

    public AppSpecificDataType getAppSpecificDataType() {
        return appSpecificDataType;
    }

    public void setAppSpecificDataType(AppSpecificDataType appSpecificDataType) {
        this.appSpecificDataType = appSpecificDataType;
    }

    public ByteOrderType getByteOrderType() {
        return byteOrderType;
    }

    public void setByteOrderType(ByteOrderType byteOrderType) {
        this.byteOrderType = byteOrderType;
    }

    public UseType getUseType() {
        return useType;
    }

    public void setUseType(UseType useType) {
        this.useType = useType;
    }

    public UseTypeType getUseTypeType() {
        return useTypeType;
    }

    public void setUseTypeType(UseTypeType useTypeType) {
        this.useTypeType = useTypeType;
    }

    public IdentifierType getIdentifierType() {
        return identifierType;
    }

    public void setIdentifierType(IdentifierType identifierType) {
        this.identifierType = identifierType;
    }

    public IdentifierTypeType getIdentifierTypeType() {
        return identifierTypeType;
    }

    public void setIdentifierTypeType(IdentifierTypeType identifierTypeType) {
        this.identifierTypeType = identifierTypeType;
    }

    public ChecksumType getChecksumType() {
        return checksumType;
    }

    public void setChecksumType(ChecksumType checksumType) {
        this.checksumType = checksumType;
    }

    public ChecksumKindType getChecksumKindType() {
        return checksumKindType;
    }

    public void setChecksumKindType(ChecksumKindType checksumKindType) {
        this.checksumKindType = checksumKindType;
    }
}
