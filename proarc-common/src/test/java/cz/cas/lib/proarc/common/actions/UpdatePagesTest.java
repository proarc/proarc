package cz.cas.lib.proarc.common.actions;

import cz.cas.lib.proarc.mods.DetailDefinition;
import cz.cas.lib.proarc.mods.GenreDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

class UpdatePagesTest {
    @Test
    void clearsTypeWithoutRemovingPageMetadata() throws Exception {
        ModsDefinition mods = page();
        setPageType(mods, "[empty]");
        assertNull(mods.getPart().get(0).getType());
        assertNull(mods.getGenre().get(0).getType());
        assertEquals("reprePage", mods.getGenre().get(0).getValue());
        assertEquals("12", mods.getPart().get(0).getDetail().get(0).getNumber().get(0).getValue());
        assertEquals("7", mods.getPart().get(1).getDetail().get(0).getNumber().get(0).getValue());
        assertEquals(2, mods.getPart().size());
    }

    @Test
    void omittedAndEmptyTypesLeaveMetadataUnchanged() throws Exception {
        for (String type : new String[] {null, ""}) {
            ModsDefinition mods = page();
            setPageType(mods, type);
            assertEquals("titlePage", mods.getPart().get(0).getType());
            assertEquals("titlePage", mods.getGenre().get(0).getType());
        }
    }

    @Test
    void setsConcretePageType() throws Exception {
        ModsDefinition mods = page();
        setPageType(mods, "normalPage");
        assertEquals("normalPage", mods.getPart().get(0).getType());
        assertEquals("normalPage", mods.getGenre().get(0).getType());
    }

    @Test
    void clearingMissingTypeDoesNotCreateEmptyParts() throws Exception {
        ModsDefinition mods = new ModsDefinition();
        setPageType(mods, "[empty]");
        assertEquals(0, mods.getPart().size());
        assertEquals(0, mods.getGenre().size());
    }

    private ModsDefinition page() {
        ModsDefinition mods = new ModsDefinition();
        mods.getPart().add(part("pageNumber", "12", "titlePage"));
        mods.getPart().add(part("pageIndex", "7", null));
        GenreDefinition genre = new GenreDefinition();
        genre.setType("titlePage");
        genre.setValue("reprePage");
        mods.getGenre().add(genre);
        return mods;
    }

    private PartDefinition part(String detailType, String value, String type) {
        PartDefinition part = new PartDefinition();
        part.setType(type);
        DetailDefinition detail = new DetailDefinition();
        detail.setType(detailType);
        StringPlusLanguage number = new StringPlusLanguage();
        number.setValue(value);
        detail.getNumber().add(number);
        part.getDetail().add(detail);
        return part;
    }

    private void setPageType(ModsDefinition mods, String type) throws Exception {
        UpdatePages updater = new UpdatePages();
        Field field = UpdatePages.class.getDeclaredField("pageType");
        field.setAccessible(true);
        field.set(updater, type);
        Method method = UpdatePages.class.getDeclaredMethod("setPageType", ModsDefinition.class, boolean.class);
        method.setAccessible(true);
        method.invoke(updater, mods, true);
    }
}
