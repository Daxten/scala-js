package java.time.chrono

import java.time.LocalTime
import java.time.temporal._
import java.{util => ju}

trait ChronoLocalDateTime[D <: ChronoLocalDate]
  extends Temporal with TemporalAdjuster with Comparable[ChronoLocalDateTime[_ <: ChronoLocalDate]] {

  import ChronoField._

  def getChronology(): Chronology = toLocalDate().getChronology

  def toLocalDate(): D

  def toLocalTime(): LocalTime

  def isSupported(field: TemporalField): Boolean

  def isSupported(unit: TemporalUnit): Boolean = unit.isSupportedBy(this)

  override def `with`(adjuster: TemporalAdjuster): ChronoLocalDateTime[D] =
    adjuster.adjustInto(this).asInstanceOf[ChronoLocalDateTime[D]]

  def `with`(field: TemporalField, value: Long): ChronoLocalDateTime[D]

  override def plus(amount: TemporalAmount): ChronoLocalDateTime[D]

  override def plus(amount: Long, unit: TemporalUnit): ChronoLocalDateTime[D]

  override def minus(amount: TemporalAmount): ChronoLocalDateTime[D]

  override def minus(amount: Long, unit: TemporalUnit): ChronoLocalDateTime[D]

  // Not implemented
  // def query[R](query: TemporalQuery[R]): R

  def adjustInto(temporal: Temporal): Temporal =
    temporal
      .`with`(EPOCH_DAY, toLocalDate().toEpochDay)
      .`with`(NANO_OF_DAY, toLocalTime().toNanoOfDay)

  // Not implemented
  // def format(formatter: java.time.format.DateFormatter): String

  // TODO
  // def atZone(zoneId: Zone): ChronoZonedDateTime[D]
  // def toInstant(offset: ZoneOffset)
  // def toEpochSecond(offset: ZoneOffset): Long

  def compareTo(other: ChronoLocalDateTime[_ <: ChronoLocalDate]): Int = {
    val dateCompare = toLocalDate().compareTo(other.toLocalDate)
    if (dateCompare == 0) toLocalTime().compareTo(other.toLocalTime)
    else dateCompare
  }

  def isAfter(other: ChronoLocalDateTime[_]): Boolean = {
    val epochDay = toLocalDate().toEpochDay
    val otherEpochDay = other.toLocalDate.asInstanceOf[ChronoLocalDate].toEpochDay
    epochDay > otherEpochDay || (epochDay == otherEpochDay && toLocalTime().toNanoOfDay > other.toLocalTime.toNanoOfDay)
  }

  def isBefore(other: ChronoLocalDateTime[_]): Boolean = {
    val epochDay = toLocalDate().toEpochDay
    val otherEpochDay = other.toLocalDate.asInstanceOf[ChronoLocalDate].toEpochDay
    epochDay < otherEpochDay || (epochDay == otherEpochDay && toLocalTime().toNanoOfDay < other.toLocalTime.toNanoOfDay)
  }

  def isEqual(other: ChronoLocalDateTime[_]): Boolean = {
    val epochDay = toLocalDate().toEpochDay
    val otherEpochDay = other.toLocalDate.asInstanceOf[ChronoLocalDate].toEpochDay
    epochDay == otherEpochDay && toLocalTime().toNanoOfDay == other.toLocalTime.toNanoOfDay
  }

  override def equals(other: Any): Boolean

  override def hashCode: Int = super.hashCode
}

object ChronoLocalDateTime {
  private val tlo = new ju.Comparator[ChronoLocalDateTime[_ <: ChronoLocalDate]] {
    def compare(date1: ChronoLocalDateTime[_ <: ChronoLocalDate],
                date2: ChronoLocalDateTime[_ <: ChronoLocalDate]): Int = {
      val compareDate = date1.toLocalDate.compareTo(date2.toLocalDate)
      if (compareDate == 0) date1.toLocalTime.compareTo(date2.toLocalTime)
      else compareDate
    }
  }

  def timeLineOrder(): ju.Comparator[ChronoLocalDateTime[_]] = tlo.asInstanceOf[ju.Comparator[ChronoLocalDateTime[_]]]

  def from(temporal: TemporalAccessor): ChronoLocalDateTime[_] = temporal match {
    case temporal: ChronoLocalDateTime[_] => temporal

    case _ =>
      // TODO: Get correct chronology (needs TemporalQuery)
      IsoChronology.INSTANCE.localDateTime(temporal)
  }
}
