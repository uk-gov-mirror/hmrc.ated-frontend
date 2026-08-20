/*
 * Copyright 2024 HM Revenue & Customs
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

package views.reliefs

import config.ApplicationConfig
import forms.ReliefForms.{reliefsForm, validateForm}
import models.{Reliefs, StandardAuthRetrievals}
import play.api.data.Form
import play.twirl.api.{Html, HtmlFormat}
import testhelpers.{AtedViewSpec, MockAuthUtil}
import views.html.reliefs.chooseReliefs

import java.time.LocalDate

class ChooseReliefsDateErrorSpec extends AtedViewSpec with MockAuthUtil {

  given appConfig: ApplicationConfig = app.injector.instanceOf[ApplicationConfig]
  given authContext: StandardAuthRetrievals = organisationStandardRetrievals

  val periodKey = 2026

  val injectedViewInstance: chooseReliefs = app.injector.instanceOf[views.html.reliefs.chooseReliefs]

  override def view: Html = viewWith(reliefsForm)

  def viewWith(form: Form[Reliefs]): Html =
    injectedViewInstance(periodKey, form, LocalDate.of(periodKey, 4, 1), HtmlFormat.empty, Some("localhost"))

  def boundWith(day: String, month: String, year: String): Form[Reliefs] =
    validateForm(reliefsForm.bind(Map(
      "periodKey"                -> periodKey.toString,
      "rentalBusiness"           -> "true",
      "rentalBusinessDate.day"   -> day,
      "rentalBusinessDate.month" -> month,
      "rentalBusinessDate.year"  -> year
    )))

  val missingYear: Form[Reliefs] = boundWith("1", "10", "")

  def classOf(form: Form[Reliefs], field: String, part: String): String =
    doc(viewWith(form)).getElementById(s"$field.$part").className()

  "The choose reliefs view" when {

    "rendered with a valid form" must {
      "style none of the rental business date inputs as errors" in {
        doc.getElementById("rentalBusinessDate.day").className() must not include "govuk-input--error"
        doc.getElementById("rentalBusinessDate.month").className() must not include "govuk-input--error"
        doc.getElementById("rentalBusinessDate.year").className() must not include "govuk-input--error"
      }
    }

    "submitted with a day and month but no year" must {

      "raise the error against the year only" in {
        missingYear.errors.map(e => (e.key, e.message)) mustBe
          Seq(("rentalBusinessDate.year", "ated.error.date.year.missing"))
      }

      "style only the year as an error" in {
        classOf(missingYear, "rentalBusinessDate", "day") must not include "govuk-input--error"
        classOf(missingYear, "rentalBusinessDate", "month") must not include "govuk-input--error"
        classOf(missingYear, "rentalBusinessDate", "year") must include("govuk-input--error")
      }

      "name the missing field in the error message" in {
        doc(viewWith(missingYear)).getElementById("rentalBusinessDate-error").text() mustBe
          "Error: Rental business start date must include the year"
      }

      "keep the input widths" in {
        classOf(missingYear, "rentalBusinessDate", "month") must include("govuk-input--width-2")
        classOf(missingYear, "rentalBusinessDate", "year") must include("govuk-input--width-4")
      }

      "leave the other reliefs' date inputs unstyled" in {
        classOf(missingYear, "openToPublicDate", "day") must not include "govuk-input--error"
        classOf(missingYear, "openToPublicDate", "month") must not include "govuk-input--error"
        classOf(missingYear, "openToPublicDate", "year") must not include "govuk-input--error"
      }
    }

    "submitted with a day only" must {
      val dayOnly = boundWith("1", "", "")

      "style the month and year but not the day" in {
        classOf(dayOnly, "rentalBusinessDate", "day") must not include "govuk-input--error"
        classOf(dayOnly, "rentalBusinessDate", "month") must include("govuk-input--error")
        classOf(dayOnly, "rentalBusinessDate", "year") must include("govuk-input--error")
      }

      "name both missing fields in the error message" in {
        doc(viewWith(dayOnly)).getElementById("rentalBusinessDate-error").text() mustBe
          "Error: Rental business start date must include the month and year"
      }
    }

    "submitted with a month out of range" must {
      val badMonth = boundWith("1", "13", periodKey.toString)

      "style only the month as an error" in {
        classOf(badMonth, "rentalBusinessDate", "day") must not include "govuk-input--error"
        classOf(badMonth, "rentalBusinessDate", "month") must include("govuk-input--error")
        classOf(badMonth, "rentalBusinessDate", "year") must not include "govuk-input--error"
      }
    }

    "submitted with nothing entered" must {
      val nothing = boundWith("", "", "")

      "style all three inputs as errors" in {
        classOf(nothing, "rentalBusinessDate", "day") must include("govuk-input--error")
        classOf(nothing, "rentalBusinessDate", "month") must include("govuk-input--error")
        classOf(nothing, "rentalBusinessDate", "year") must include("govuk-input--error")
      }
    }

    "the date falls outside the chargeable period" must {
      val outOfPeriod = boundWith("1", "4", (periodKey - 1).toString)

      "style all three inputs as errors" in {
        classOf(outOfPeriod, "rentalBusinessDate", "day") must include("govuk-input--error")
        classOf(outOfPeriod, "rentalBusinessDate", "month") must include("govuk-input--error")
        classOf(outOfPeriod, "rentalBusinessDate", "year") must include("govuk-input--error")
      }
    }
  }
}
