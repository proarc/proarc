/*
 * Copyright (C) 2013 Robert Simonovsky
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package cz.cas.lib.proarc.common.export.mets;

import com.yourmediashelf.fedora.generated.foxml.DigitalObject;

import cz.cas.lib.proarc.common.export.mets.structure.MetsEntity;
import cz.cas.lib.proarc.common.export.mets.structure.MetsInfo;

public class FoXMLConverter {

    public static void main(String[] args) {
	// DigitalObject foXML =
	// Utils.readFoXML("/work/incad_docs/vlada/monografie/44589055-9fad-4a9f-b6a8-75be399f332d.xml");

//	DigitalObject foXML =
//	 Utils.readFoXML("/work/incad_docs/vlada/periodikum/converted/3733b6e3-61ab-42fc-a437-964d143acc45.xml");
	DigitalObject foXML =
	Utils.readFoXML("/work/incad_docs/vlada/clanky/converted/5d086dcf-94b7-4914-9b3e-858076e3884f.xml");
	// DigitalObject foXML =
	// Utils.readFoXML("/work/incad_docs/vlada/eskymo/converted/9f963b88-c186-4c4b-a403-1f2e8bfd9208.xml");

	//FedoraClient fedoraClient = Test.fedoraClientSetup();
	//DigitalObject foXML = Utils.readFoXML("uuid:3733b6e3-61ab-42fc-a437-964d143acc45", fedoraClient);
//	MetsInfo metsInfo = new MetsEntity(foXML, fedoraClient, "anl-22234");
	MetsInfo metsInfo = new MetsEntity(foXML,"/work/incad_docs/vlada/clanky/converted/","xxx");

	metsInfo.insertIntoMets("/work/incad_docs/vlada/newPeriodikum", true);
	metsInfo.save();
	// Utils.saveMets("/work/incad_docs/vlada/newPeriodikum/pokus_mets.xml",
	// metsInfo.getMets());
    }
}
