package cz.cas.lib.proarc.foxml.utility;

import java.text.DecimalFormat;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.util.Date;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Date and time utility methods compatible with Java 21.
 *
 * Replaces Joda-Time with java.time API.
 *
 * @author Edwin Shin
 */
public class DateUtility {

    private static final String yearFrag = "(-?([1-9][0-9]{3,}|0[0-9]{3}))";
    private static final String monthFrag = "(0[1-9]|1[0-2])";
    private static final String dayFrag = "(0[1-9]|[12][0-9]|3[01])";
    private static final String hourFrag = "([01][0-9]|2[0-3])";
    private static final String minuteFrag = "([0-5][0-9])";
    private static final String secondFrag = "([0-5][0-9])(\\.([0-9]+))?";
    private static final String endOfDayFrag = String.format("(%s:%s:%s|(24:00:00(\\.0+)?))", hourFrag, minuteFrag, secondFrag);
    private static final String timezoneFrag = "(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?";
    private static final String combined = String.format("%s-%s-%sT%s%s", yearFrag, monthFrag, dayFrag, endOfDayFrag, timezoneFrag);

    private static final Pattern XSD_DATETIME = Pattern.compile(combined);
    private static final ConcurrentMap<Integer, DateTimeFormatter> formatters = new ConcurrentHashMap<>();

    public static OffsetDateTime parseXSDDateTime(String input) {
        Matcher m = XSD_DATETIME.matcher(input);
        if (!m.find()) {
            throw new IllegalArgumentException(input + " is not a valid XML Schema 1.1 dateTime.");
        }

        int year = Integer.parseInt(m.group(1));
        int month = Integer.parseInt(m.group(3));
        int day = Integer.parseInt(m.group(4));

        int hour = 0, minute = 0, second = 0, millis = 0;
        boolean hasEndOfDayFrag = m.group(11) != null;
        if (!hasEndOfDayFrag) {
            hour = Integer.parseInt(m.group(6));
            minute = Integer.parseInt(m.group(7));
            second = Integer.parseInt(m.group(8));
            if (m.group(9) != null && !m.group(9).isEmpty()) {
                double d = Double.parseDouble(m.group(9));
                millis = (int) (d * 1000);
            }
        }

        ZoneOffset offset = ZoneOffset.UTC;
        if (m.group(13) != null) {
            String tmp = m.group(13);
            if ("Z".equals(tmp)) tmp = "+00:00";
            offset = ZoneOffset.of(tmp);
        }

        OffsetDateTime dt = OffsetDateTime.of(year, month, day, hour, minute, second, millis * 1_000_000, offset);
        if (hasEndOfDayFrag) {
            dt = dt.plusDays(1);
        }
        return dt;
    }

    public static String getXSDDateTime(Date date) {
        return getXSDDateTime(date.toInstant().atOffset(ZoneOffset.UTC));
    }

    public static String getXSDDateTime(OffsetDateTime dateTime) {
        int millis = dateTime.getNano() / 1_000_000;
        return dateTime.withOffsetSameInstant(ZoneOffset.UTC).format(getXSDFormatter(millis));
    }

    public static DateTimeFormatter getXSDFormatter(int millis) {
        int precision = millis > 0 ? String.valueOf(new DecimalFormat(".###").format(millis / 1000.0)).replace("0.", "").length() : 0;
        return formatters.computeIfAbsent(precision, p -> {
            DateTimeFormatterBuilder bldr = new DateTimeFormatterBuilder()
                    .appendValue(ChronoField.YEAR, 4)
                    .appendLiteral('-')
                    .appendValue(ChronoField.MONTH_OF_YEAR, 2)
                    .appendLiteral('-')
                    .appendValue(ChronoField.DAY_OF_MONTH, 2)
                    .appendLiteral('T')
                    .appendValue(ChronoField.HOUR_OF_DAY, 2)
                    .appendLiteral(':')
                    .appendValue(ChronoField.MINUTE_OF_HOUR, 2)
                    .appendLiteral(':')
                    .appendValue(ChronoField.SECOND_OF_MINUTE, 2);
            if (p > 0) {
                bldr.appendLiteral('.').appendFraction(ChronoField.MILLI_OF_SECOND, p, p, true);
            }
            bldr.appendOffset("+HH:MM", "Z");
            return bldr.toFormatter();
        });
    }
}