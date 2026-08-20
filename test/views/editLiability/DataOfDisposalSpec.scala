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

package views.editLiability

import config.ApplicationConfig
import forms.AtedForms.disposeLiabilityForm
import models.{DisposeLiability, StandardAuthRetrievals}
import play.api.data.Form
import play.twirl.api.{Html, HtmlFormat}
import testhelpers.{AtedViewSpec, MockAuthUtil}
import views.html.editLiability.dataOfDisposal

class DataOfDisposalSpec extends AtedViewSpec with MockAuthUtil {

  given appConfig: ApplicationConfig = app.injector.instanceOf[ApplicationConfig]
  given authContext: StandardAuthRetrievals = organisationStandardRetrievals

  val injectedViewInstance: dataOfDisposal = app.injector.instanceOf[views.html.editLiability.dataOfDisposal]

  val messageKey: String = messages("ated.property-details-value.dateOfDisposal.messageKey")

  override def view: Html = injectedViewInstance(disposeLiabilityForm, "12345678901", HtmlFormat.empty, Some("localhost"), 2026)

  def viewWith(form: Form[DisposeLiability]): Html =
    injectedViewInstance(form, "12345678901", HtmlFormat.empty, Some("localhost"), 2026)

  "The date of disposal view" when {

    "rendered with a valid form" must {
      "style none of the inputs as errors" in {
        doc.getElementById("dateOfDisposal.day").className() must not include "govuk-input--error"
        doc.getElementById("dateOfDisposal.month").className() must not include "govuk-input--error"
        doc.getElementById("dateOfDisposal.year").className() must not include "govuk-input--error"
      }
    }

    "submitted with only a day" must {
      val form = disposeLiabilityForm.withError("dateOfDisposal.month", "ated.error.date.monthyear.missing", messageKey)

      "style the month and year as errors and leave the day unstyled" in {
        doc(viewWith(form)).getElementById("dateOfDisposal.day").className() must not include "govuk-input--error"
        doc(viewWith(form)).getElementById("dateOfDisposal.month").className() must include("govuk-input--error")
        doc(viewWith(form)).getElementById("dateOfDisposal.year").className() must include("govuk-input--error")
      }
    }

    "submitted with nothing entered" must {
      val form = disposeLiabilityForm.withError("dateOfDisposal", "ated.error.date.empty", messageKey)

      "style all three inputs as errors" in {
        doc(viewWith(form)).getElementById("dateOfDisposal.day").className() must include("govuk-input--error")
        doc(viewWith(form)).getElementById("dateOfDisposal.month").className() must include("govuk-input--error")
        doc(viewWith(form)).getElementById("dateOfDisposal.year").className() must include("govuk-input--error")
      }
    }
  }
}
