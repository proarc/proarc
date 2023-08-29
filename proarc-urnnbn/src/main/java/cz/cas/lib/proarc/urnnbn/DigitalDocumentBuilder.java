/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.urnnbn;

import cz.cas.lib.proarc.mix.*;
import cz.cas.lib.proarc.mix.BasicDigitalObjectInformationType.FormatDesignation;
import cz.cas.lib.proarc.mix.BasicImageInformationType.BasicImageCharacteristics;
import cz.cas.lib.proarc.mix.ImageAssessmentMetadataType.SpatialMetrics;
import cz.cas.lib.proarc.urnnbn.model.registration.*;

import java.math.BigInteger;
import java.util.List;

/**
 * Builds a digital document for the resolver registration.
 *
 * @author Jan Pokorsky
 */
public class DigitalDocumentBuilder {

    private DigitalDocument digitalDocument = new DigitalDocument();

    public DigitalDocument build() {
        return digitalDocument;
    }

    public DigitalDocumentBuilder setUuid(String uuid) {
        RegistrarScopeIdentifiers rsIdentifiers = new RegistrarScopeIdentifiers();
        RegistrarScopeIdentifier rsIdentifier = new RegistrarScopeIdentifier();
        rsIdentifier.setType("uuid");
        rsIdentifier.setValue(uuid);
        rsIdentifiers.getId().add(rsIdentifier);
        digitalDocument.setRegistrarScopeIdentifiers(rsIdentifiers);
        return this;
    }

    public DigitalDocumentBuilder setCompression(String scheme, Double ratio) {
        if (scheme != null || ratio != null) {
            Compression tmCompression = new Compression();
            tmCompression.setValue(scheme);
            tmCompression.setRatio(ratio);
            getTechnicalMetadata().setCompression(tmCompression);
        }
        return this;
    }

    public DigitalDocumentBuilder setFormat(String formatName, String formatVersion) {
        if (formatName != null || formatVersion != null) {
            Format format = new Format();
            format.setValue(formatName);
            format.setVersion(formatVersion);
            getTechnicalMetadata().setFormat(format);
        }
        return this;
    }

    public DigitalDocumentBuilder setPreccessor(String urnNbnValue) {
        if (urnNbnValue != null && !urnNbnValue.isEmpty()) {
            DigitalDocument.UrnNbn urnNbn = new DigitalDocument.UrnNbn();
            Predecessor predecessor = new Predecessor();
            urnNbn.getPredecessor().add(predecessor);

            predecessor.setUrnNbnValue(urnNbnValue);
            predecessor.setNote("Úprava metadat objektu.");
            digitalDocument.setUrnNbn(urnNbn);
        }
        return this;
    }

    public DigitalDocumentBuilder setMix(MixType mix) {
        BasicDigitalObjectInformationType basicDigitalObjectInformation = mix.getBasicDigitalObjectInformation();
        if (basicDigitalObjectInformation != null) {
            // format
            FormatDesignation formatDesignation = basicDigitalObjectInformation.getFormatDesignation();
            if (formatDesignation != null) {
                setFormat(string(formatDesignation.getFormatName()), string(formatDesignation.getFormatVersion()));
            }

            // compression
            List<BasicDigitalObjectInformationType.Compression> compressions = basicDigitalObjectInformation.getCompression();
            if (!compressions.isEmpty()) {
                BasicDigitalObjectInformationType.Compression compression = compressions.get(0);
                String scheme = string(compression.getCompressionScheme());
                BigInteger ratio = ratio(compression.getCompressionRatio());
                setCompression(scheme, ratio == null ? null : ratio.doubleValue());
            }
        }

        // size
        BasicImageInformationType basicImageInformation = mix.getBasicImageInformation();
        if (basicImageInformation != null) {
            BasicImageCharacteristics basicImageCharacteristics = basicImageInformation.getBasicImageCharacteristics();
            if (basicImageCharacteristics != null) {
                PositiveIntegerType imageHeight = basicImageCharacteristics.getImageHeight();
                BigInteger height = imageHeight == null ? null : imageHeight.getValue();
                PositiveIntegerType imageWidth = basicImageCharacteristics.getImageWidth();
                BigInteger width = imageWidth == null ? null : imageWidth.getValue();
                if (height != null || width == null) {
                    PictureSize pictureSize = new PictureSize();
                    pictureSize.setHeight(height);
                    pictureSize.setWidth(width);
                    getTechnicalMetadata().setPictureSize(pictureSize);
                }
            }
        }

        // resolution
        ImageAssessmentMetadataType imageAssessmentMetadata = mix.getImageAssessmentMetadata();
        if (imageAssessmentMetadata != null) {
            SpatialMetrics spatialMetrics = imageAssessmentMetadata.getSpatialMetrics();
            if (spatialMetrics != null) {
                BigInteger xRatio = ratio(spatialMetrics.getXSamplingFrequency());
                BigInteger yRatio = ratio(spatialMetrics.getYSamplingFrequency());
                if (xRatio != null || yRatio != null) {
                    Resolution resolution = new Resolution();
                    resolution.setHorizontal(xRatio);
                    resolution.setVertical(yRatio);
                    getTechnicalMetadata().setResolution(resolution);
                }
            }
            ImageAssessmentMetadataType.ImageColorEncoding imageColorEncoding = imageAssessmentMetadata.getImageColorEncoding();
            if (imageColorEncoding != null) {
                ImageAssessmentMetadataType.ImageColorEncoding.BitsPerSample bitsPerSample = imageColorEncoding.getBitsPerSample();
                if (bitsPerSample != null) {
                    for (PositiveIntegerType depth : bitsPerSample.getBitsPerSampleValue()) {
                        if (depth != null && depth.getValue() != null) {
                            Color color = getTechnicalMetadata().getColor();
                            if (color == null) {
                                color = new Color();
                                getTechnicalMetadata().setColor(color);
                            }
                            color.setDepth(depth.getValue());

                            break;
                        }
                    }
                }
            }
        }
        return this;
    }

    private TechnicalMetadata getTechnicalMetadata() {
        TechnicalMetadata tm = digitalDocument.getTechnicalMetadata();
        if (tm == null) {
            tm = new TechnicalMetadata();
            digitalDocument.setTechnicalMetadata(tm);
        }
        return tm;
    }

    private static String string(StringType st) {
        return st == null ? null : st.getValue();
    }

    private static BigInteger ratio(BigInteger numerator, BigInteger denominator) {
        if (numerator != null && denominator == null) {
            return numerator;
        }
        if (numerator != null && denominator != null) {
            return numerator.divide(denominator);
        }
        return null;
    }

    private static BigInteger ratio(RationalType rationalType) {
        if (rationalType != null) {
            return ratio(rationalType.getNumerator(), rationalType.getDenominator());
        }
        return null;
    }
}
