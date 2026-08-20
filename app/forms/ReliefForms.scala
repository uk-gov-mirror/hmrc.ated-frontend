/*
 * Copyright 2023 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package forms

import forms.mappings.DateTupleCustomError
import models.*
import java.time.LocalDate
import play.api.data.Forms.*
import play.api.data.validation.{Constraint, Invalid, Valid}
import play.api.data.{Form, FormError, Mapping}
import play.api.i18n.Messages
import utils.PeriodUtils.*
import scala.annotation.tailrec
import scala.util.Try

object ReliefForms {

  val numRegex = """[0-9]{8}"""

  val avoidanceSchemeConstraint: Constraint[IsTaxAvoidance] = Constraint("isAvoidanceScheme")({
    model =>
      if (model.isAvoidanceScheme.isDefined) {
        Valid
      } else {
        Invalid("ated.claim-relief.avoidance-scheme.selected", "isAvoidanceScheme")
      }
  })

  val reliefSelectedConstraint: Constraint[Reliefs] = Constraint("rentalBusiness")({
    model =>
      if (model.rentalBusiness || model.openToPublic
        || model.propertyDeveloper || model.propertyTrading || model.lending || model.employeeOccupation
        || model.farmHouses || model.socialHousing || model.equityRelease) {
        Valid
      } else {
        Invalid("ated.choose-reliefs.error", "rentalBusiness")
      }
  })

  val reliefsForm: Form[Reliefs] = Form(mapping(
    "periodKey" -> number,
    "rentalBusiness" -> boolean,
    "rentalBusinessDate" -> dateTuple(),
    "openToPublic" -> boolean,
    "openToPublicDate" -> dateTuple(),
    "propertyDeveloper" -> boolean,
    "propertyDeveloperDate" -> dateTuple(),
    "propertyTrading" -> boolean,
    "propertyTradingDate" -> dateTuple(),
    "lending" -> boolean,
    "lendingDate" -> dateTuple(),
    "employeeOccupation" -> boolean,
    "employeeOccupationDate" -> dateTuple(),
    "farmHouses" -> boolean,
    "farmHousesDate" -> dateTuple(),
    "socialHousing" -> boolean,
    "socialHousingDate" -> dateTuple(),
    "equityRelease" -> boolean,
    "equityReleaseDate" -> dateTuple(),
    "isAvoidanceScheme" -> optional(boolean)
  )
    (Reliefs.apply)(x => Some(Tuple.fromProductTyped(x)))
    .verifying(reliefSelectedConstraint)
  )

  val fields = Seq(
    ("rentalBusiness", "rentalBusinessDate"),
    ("openToPublic", "openToPublicDate"),
    ("propertyDeveloper", "propertyDeveloperDate"),
    ("propertyTrading", "propertyTradingDate"),
    ("lending", "lendingDate"),
    ("employeeOccupation", "employeeOccupationDate"),
    ("farmHouses", "farmHousesDate"),
    ("socialHousing", "socialHousingDate"),
    ("equityRelease", "equityReleaseDate")
  )

  private[forms] def dateMessageSuffix(periodKey: Int, dateField: String): String =
    if (dateField == "socialHousingDate" && periodKey >= 2020) "providerSocialOrHousingDate" else dateField

  private def reliefDate(f: Form[Reliefs], dateField: String): Option[LocalDate] =
    (f.data.get(s"$dateField.day"), f.data.get(s"$dateField.month"), f.data.get(s"$dateField.year")) match {
      case (Some(d), Some(m), Some(y)) => Try(LocalDate.of(y.trim.toInt, m.trim.toInt, d.trim.toInt)).toOption
      case _ => None
    }

  def validateForm(f: Form[Reliefs])(implicit messages: Messages): Form[Reliefs] = {
    val periodKey = f.data.get("periodKey").get.toInt

    val formErrors = fields.flatMap { case (reliefField, dateField) =>
      if (f.data.get(reliefField).contains("true")) {
        val messageSuffix = dateMessageSuffix(periodKey, dateField)

        val dateErrors = DateTupleCustomError.validateDateFields(
          f.data.get(s"$dateField.day").orElse(Some("")),
          f.data.get(s"$dateField.month").orElse(Some("")),
          f.data.get(s"$dateField.year").orElse(Some("")),
          Seq((dateField, messages(s"ated.choose-reliefs.messageKey.$messageSuffix")))
        )

        if (dateErrors.nonEmpty) dateErrors
        else reliefDate(f, dateField) match {
          case Some(date) if isPeriodTooEarly(periodKey, Some(date)) || isPeriodTooLate(periodKey, Some(date)) =>
            Seq(FormError(dateField, s"ated.choose-reliefs.error.date.chargePeriod.$messageSuffix"))
          case _ => Nil
        }
      } else Nil
    }

    addErrorsToForm(f, formErrors)
  }

  val isTaxAvoidanceForm: Form[IsTaxAvoidance] = Form(mapping(

    "isAvoidanceScheme" -> optional(boolean)
  )
    (IsTaxAvoidance.apply)(x => Some(x.isAvoidanceScheme))
    .verifying(avoidanceSchemeConstraint)
  )

  val taxAvoidanceForm: Form[TaxAvoidance] = Form(mapping(
    "rentalBusinessScheme" -> optional(text),
    "rentalBusinessSchemePromoter" -> optional(text),
    "openToPublicScheme" -> optional(text),
    "openToPublicSchemePromoter" -> optional(text),
    "propertyDeveloperScheme" -> optional(text),
    "propertyDeveloperSchemePromoter" -> optional(text),
    "propertyTradingScheme" -> optional(text),
    "propertyTradingSchemePromoter" -> optional(text),
    "lendingScheme" -> optional(text),
    "lendingSchemePromoter" -> optional(text),
    "employeeOccupationScheme" -> optional(text),
    "employeeOccupationSchemePromoter" -> optional(text),
    "farmHousesScheme" -> optional(text),
    "farmHousesSchemePromoter" -> optional(text),
    "socialHousingScheme" -> optional(text),
    "socialHousingSchemePromoter" -> optional(text),
    "equityReleaseScheme" -> optional(text),
    "equityReleaseSchemePromoter" -> optional(text)
  )
    (TaxAvoidance.apply)(x => Some(Tuple.fromProductTyped(x)))
  )

  //scalastyle:off cyclomatic.complexity
  def validateTaxAvoidance(f: Form[TaxAvoidance], periodKey: Int): Form[TaxAvoidance] = {
    def validateAvoidanceScheme(avoidanceFieldName: String): Seq[Option[FormError]] = {
      val messageKeySuffix = if (periodKey >= 2020 && avoidanceFieldName == "socialHousingScheme") "providerSocialOrHousingScheme" else avoidanceFieldName
      val avoidanceSchemeNo = f.data.get(avoidanceFieldName)
      avoidanceSchemeNo.getOrElse("") match {
        case a if a.isEmpty => Seq(Some(FormError(avoidanceFieldName, s"ated.avoidance-scheme-error.general.empty.$messageKeySuffix")))
        case a if Try(a.toInt).isFailure => Seq(Some(FormError(avoidanceFieldName, s"ated.avoidance-scheme-error.general.numeric-error.$messageKeySuffix")))
        case a if a.length != 8 => Seq(Some(FormError(avoidanceFieldName, s"ated.avoidance-scheme-error.general.wrong-length.$messageKeySuffix")))
        case _ => Seq(None)
      }
    }

    def validatePromoterReference(promoterFieldName: String): Seq[Option[FormError]] = {
      val messageKeySuffix = if (periodKey >= 2020 && promoterFieldName == "socialHousingSchemePromoter") "providerSocialOrHousingSchemePromoter" else promoterFieldName
      val promoterReference = f.data.get(promoterFieldName)
      promoterReference.getOrElse("") match {
        case a if a.isEmpty => Seq(Some(FormError(promoterFieldName, s"ated.avoidance-scheme-error.general.empty.$messageKeySuffix")))
        case a if Try(a.toInt).isFailure => Seq(Some(FormError(promoterFieldName, s"ated.avoidance-scheme-error.general.numeric-error.$messageKeySuffix")))
        case a if a.length != 8 => Seq(Some(FormError(promoterFieldName, s"ated.avoidance-scheme-error.general.wrong-length.$messageKeySuffix")))
        case _ => Seq(None)
      }
    }

    def validateAvoidance(avoidanceFieldName: String, promoterFieldName: String): Seq[Option[FormError]] = {
      val avoidanceValue = f.data.get(avoidanceFieldName)
      val promoterValue = f.data.get(promoterFieldName)

      if (!avoidanceValue.getOrElse("").trim.isEmpty || !promoterValue.getOrElse("").trim.isEmpty) {
        validateAvoidanceScheme(avoidanceFieldName) ++ validatePromoterReference(promoterFieldName)
      } else {
        Seq(None)
      }
    }

    if (!f.hasErrors) {
      validateTA(f.value.getOrElse(TaxAvoidance())) match {
        case true =>
          val errors =
            validateAvoidance("rentalBusinessScheme", "rentalBusinessSchemePromoter") ++
              validateAvoidance("openToPublicScheme", "openToPublicSchemePromoter") ++
              validateAvoidance("propertyDeveloperScheme", "propertyDeveloperSchemePromoter") ++
              validateAvoidance("propertyTradingScheme", "propertyTradingSchemePromoter") ++
              validateAvoidance("lendingScheme", "lendingSchemePromoter") ++
              validateAvoidance("employeeOccupationScheme", "employeeOccupationSchemePromoter") ++
              validateAvoidance("farmHousesScheme", "farmHousesSchemePromoter") ++
              validateAvoidance("socialHousingScheme", "socialHousingSchemePromoter") ++
              validateAvoidance("equityReleaseScheme", "equityReleaseSchemePromoter")

          addErrorsToForm(f, errors.flatten)
        case false => f.withError("", "ated.avoidance-schemes.scheme.empty") // message parameter doesn't matter as we get a message using the error key
      }
    } else f
  }

  private def validateTA(ta: TaxAvoidance): Boolean = {
    List(
      ta.employeeOccupationScheme, ta.employeeOccupationSchemePromoter,
      ta.farmHousesScheme, ta.farmHousesSchemePromoter,
      ta.lendingScheme, ta.lendingSchemePromoter,
      ta.openToPublicScheme, ta.openToPublicSchemePromoter,
      ta.propertyDeveloperScheme, ta.propertyDeveloperSchemePromoter,
      ta.propertyTradingScheme, ta.propertyTradingSchemePromoter,
      ta.rentalBusinessScheme, ta.rentalBusinessSchemePromoter,
      ta.socialHousingScheme, ta.socialHousingSchemePromoter,
      ta.equityReleaseScheme, ta.equityReleaseSchemePromoter
    ).flatten.nonEmpty
  }

  def addErrorsToForm[A](form: Form[A], formErrors: Seq[FormError]): Form[A] = {
    @tailrec
    def y(f: Form[A], fe: Seq[FormError]): Form[A] = {
      if (fe.isEmpty) f
      else y(f.withError(fe.head), fe.tail)
    }

    y(form, formErrors)
  }

  private def dateTuple(): Mapping[Option[LocalDate]] =
    tuple(
      "year" -> optional(text),
      "month" -> optional(text),
      "day" -> optional(text)
    ).transform(
      {
        case (Some(y), Some(m), Some(d)) =>
          try Some(LocalDate.of(y.trim.toInt, m.trim.toInt, d.trim.toInt))
          catch {
            case _: Exception => None
          }
        case (a, b, c) => None
      },
      (date: Option[LocalDate]) =>
        date match {
          case Some(d) => (Some(d.getYear.toString), Some(d.getMonthValue().toString), Some(d.getDayOfMonth.toString))
          case _ => (None, None, None)
        }
    )
}
