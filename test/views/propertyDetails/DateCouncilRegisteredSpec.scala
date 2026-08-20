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

package views.propertyDetails

import config.ApplicationConfig
import forms.PropertyDetailsForms
import forms.PropertyDetailsForms.dateCouncilRegisteredForm
import models.{DateCouncilRegistered, StandardAuthRetrievals}
import play.api.data.Form
import play.twirl.api.{Html, HtmlFormat}
import testhelpers.{AtedViewSpec, MockAuthUtil}
import views.html.propertyDetails.dateCouncilRegistered

class DateCouncilRegisteredSpec extends AtedViewSpec with MockAuthUtil {

  given appConfig: ApplicationConfig = app.injector.instanceOf[ApplicationConfig]
  given authContext: StandardAuthRetrievals = organisationStandardRetrievals

  val injectedViewInstance: dateCouncilRegistered = app.injector.instanceOf[views.html.propertyDetails.dateCouncilRegistered]

  val messageKey: String = messages("ated.property-details.council-registered-date.messageKey")

  override def view: Html = injectedViewInstance("anything", 2026, dateCouncilRegisteredForm, None, HtmlFormat.empty, Some("localhost"))

  def viewWith(form: Form[DateCouncilRegistered]): Html =
    injectedViewInstance("anything", 2026, form, None, HtmlFormat.empty, Some("localhost"))

  "The when did the local council register the property for council tax view" when {

    "rendered with a valid form" must {
      "style none of the inputs as errors" in {
        doc.getElementById("dateCouncilRegistered.day").className() must not include "govuk-input--error"
        doc.getElementById("dateCouncilRegistered.month").className() must not include "govuk-input--error"
        doc.getElementById("dateCouncilRegistered.year").className() must not include "govuk-input--error"
      }
    }

    "submitted with only a day" must {
      val form = dateCouncilRegisteredForm.withError("dateCouncilRegistered.month", "ated.error.date.monthyear.missing", messageKey)

      "style the month and year as errors" in {
        doc(viewWith(form)).getElementById("dateCouncilRegistered.month").className() must include("govuk-input--error")
        doc(viewWith(form)).getElementById("dateCouncilRegistered.year").className() must include("govuk-input--error")
      }

      "leave the day unstyled" in {
        doc(viewWith(form)).getElementById("dateCouncilRegistered.day").className() must not include "govuk-input--error"
      }

      "keep the input widths" in {
        doc(viewWith(form)).getElementById("dateCouncilRegistered.month").className() must include("govuk-input--width-2")
        doc(viewWith(form)).getElementById("dateCouncilRegistered.year").className() must include("govuk-input--width-4")
      }
    }

    "submitted with only a year" must {
      val form = dateCouncilRegisteredForm.withError("dateCouncilRegistered.day", "ated.error.date.daymonth.missing", messageKey)

      "style the day and month as errors and leave the year unstyled" in {
        doc(viewWith(form)).getElementById("dateCouncilRegistered.day").className() must include("govuk-input--error")
        doc(viewWith(form)).getElementById("dateCouncilRegistered.month").className() must include("govuk-input--error")
        doc(viewWith(form)).getElementById("dateCouncilRegistered.year").className() must not include "govuk-input--error"
      }
    }

    "submitted with only a month" must {
      val form = dateCouncilRegisteredForm.withError("dateCouncilRegistered.day", "ated.error.date.dayyear.missing", messageKey)

      "style the day and year as errors and leave the month unstyled" in {
        doc(viewWith(form)).getElementById("dateCouncilRegistered.day").className() must include("govuk-input--error")
        doc(viewWith(form)).getElementById("dateCouncilRegistered.month").className() must not include "govuk-input--error"
        doc(viewWith(form)).getElementById("dateCouncilRegistered.year").className() must include("govuk-input--error")
      }
    }

    "submitted with nothing entered" must {
      val form = dateCouncilRegisteredForm.withError("dateCouncilRegistered", "ated.error.date.empty", messageKey)

      "style all three inputs as errors" in {
        doc(viewWith(form)).getElementById("dateCouncilRegistered.day").className() must include("govuk-input--error")
        doc(viewWith(form)).getElementById("dateCouncilRegistered.month").className() must include("govuk-input--error")
        doc(viewWith(form)).getElementById("dateCouncilRegistered.year").className() must include("govuk-input--error")
      }
    }

    "bound through the real validation with a day but no month or year" must {
      val bound = PropertyDetailsForms.validateNewBuildCouncilRegisteredDate(
        2026,
        dateCouncilRegisteredForm.bind(Map(
          "dateCouncilRegistered.day"   -> "1",
          "dateCouncilRegistered.month" -> "",
          "dateCouncilRegistered.year"  -> ""
        )),
        Seq(("dateCouncilRegistered", messageKey))
      )

      "raise the month and year error against the month key" in {
        bound.errors.map(e => (e.key, e.message)) mustBe
          Seq(("dateCouncilRegistered.month", "ated.error.date.monthyear.missing"))
      }

      "style both the month and the year inputs as errors" in {
        doc(viewWith(bound)).getElementById("dateCouncilRegistered.day").className() must not include "govuk-input--error"
        doc(viewWith(bound)).getElementById("dateCouncilRegistered.month").className() must include("govuk-input--error")
        doc(viewWith(bound)).getElementById("dateCouncilRegistered.year").className() must include("govuk-input--error")
      }

      "show the error message naming both fields" in {
        doc(viewWith(bound)).getElementById("dateCouncilRegistered-error").text() mustBe
          "Error: Date when the local council registered the property for council tax must include the month and year"
      }
    }
  }
}
