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

package views

import org.scalatestplus.play.PlaySpec
import play.api.data.Form
import play.api.data.Forms.{optional, single, text}
import uk.gov.hmrc.govukfrontend.views.viewmodels.dateinput.{DateInput, InputItem}

class DateInputErrorsSpec extends PlaySpec {

  val dateField = "dateCouncilRegistered"

  val testForm: Form[Option[String]] = Form(single("dateCouncilRegistered" -> optional(text)))

  def formWithError(key: String, message: String): Form[Option[String]] =
    testForm.withError(key, message)

  "DateInputErrors.fieldsInError" must {

    "highlight nothing when the form has no errors" in {
      DateInputErrors.fieldsInError(testForm, dateField) mustBe Set.empty
    }

    "highlight only the day when the day is missing" in {
      DateInputErrors.fieldsInError(
        formWithError(s"$dateField.day", "ated.error.date.day.missing"), dateField
      ) mustBe Set("day")
    }

    "highlight only the month when the month is missing" in {
      DateInputErrors.fieldsInError(
        formWithError(s"$dateField.month", "ated.error.date.month.missing"), dateField
      ) mustBe Set("month")
    }

    "highlight only the year when the year is missing" in {
      DateInputErrors.fieldsInError(
        formWithError(s"$dateField.year", "ated.error.date.year.missing"), dateField
      ) mustBe Set("year")
    }

    "highlight the day and month when both are missing" in {
      DateInputErrors.fieldsInError(
        formWithError(s"$dateField.day", "ated.error.date.daymonth.missing"), dateField
      ) mustBe Set("day", "month")
    }

    "highlight the day and year when both are missing" in {
      DateInputErrors.fieldsInError(
        formWithError(s"$dateField.day", "ated.error.date.dayyear.missing"), dateField
      ) mustBe Set("day", "year")
    }

    "highlight the month and year when both are missing, even though the error is bound to the month" in {
      DateInputErrors.fieldsInError(
        formWithError(s"$dateField.month", "ated.error.date.monthyear.missing"), dateField
      ) mustBe Set("month", "year")
    }

    "highlight the day and month when the day is not valid for the month" in {
      DateInputErrors.fieldsInError(
        formWithError(s"$dateField.day", "ated.error.date.invalid.day.month"), dateField
      ) mustBe Set("day", "month")
    }

    "highlight only the day when the day is out of range" in {
      DateInputErrors.fieldsInError(
        formWithError(s"$dateField.day", "ated.error.day.invalid"), dateField
      ) mustBe Set("day")
    }

    "highlight only the month when the month is out of range" in {
      DateInputErrors.fieldsInError(
        formWithError(s"$dateField.month", "ated.error.month.invalid"), dateField
      ) mustBe Set("month")
    }

    "highlight only the year when the year is not 4 digits" in {
      DateInputErrors.fieldsInError(
        formWithError(s"$dateField.year", "ated.error.date.year.length"), dateField
      ) mustBe Set("year")
    }

    "highlight only the year when the year is out of range" in {
      DateInputErrors.fieldsInError(
        formWithError(s"$dateField.year", "ated.error.date.notInRange"), dateField
      ) mustBe Set("year")
    }

    "highlight the whole date when the date is empty" in {
      DateInputErrors.fieldsInError(
        formWithError(dateField, "ated.error.date.empty"), dateField
      ) mustBe DateInputErrors.AllFields
    }

    "highlight the whole date for an unmapped message bound to a sub field" in {
      DateInputErrors.fieldsInError(
        formWithError(s"$dateField.day", "ated.error.date.invalid"), dateField
      ) mustBe DateInputErrors.AllFields
    }

    "highlight the whole date when the date is in the future" in {
      DateInputErrors.fieldsInError(
        formWithError(s"$dateField.day", "ated.error.date.future"), dateField
      ) mustBe DateInputErrors.AllFields
    }

    "combine the fields when more than one error is present" in {
      val form = testForm
        .withError(s"$dateField.day", "ated.error.day.invalid")
        .withError(s"$dateField.year", "ated.error.date.notInRange")

      DateInputErrors.fieldsInError(form, dateField) mustBe Set("day", "year")
    }

    "ignore errors belonging to a different date on the same page" in {
      DateInputErrors.fieldsInError(
        formWithError("endDate.month", "ated.error.date.monthyear.missing"), "startDate"
      ) mustBe Set.empty
    }

    "not treat a field whose name merely starts with the date field as part of that date" in {
      DateInputErrors.fieldsInError(
        formWithError(s"${dateField}Known", "ated.error.date.day.missing"), dateField
      ) mustBe Set.empty
    }
  }

  "DateInputErrors.highlightErrors" must {

    def dateInputWith(dayClasses: String, monthClasses: String, yearClasses: String): DateInput =
      DateInput(items = Seq(
        InputItem(id = s"$dateField.day", name = s"$dateField.day", classes = dayClasses),
        InputItem(id = s"$dateField.month", name = s"$dateField.month", classes = monthClasses),
        InputItem(id = s"$dateField.year", name = s"$dateField.year", classes = yearClasses)
      ))

    val cleanInput = dateInputWith("govuk-input--width-2", "govuk-input--width-2", "govuk-input--width-4")

    "add the error class to the month and year but not the day" in {
      val result = DateInputErrors.highlightErrors(
        cleanInput, formWithError(s"$dateField.month", "ated.error.date.monthyear.missing"), dateField
      )

      result.items.map(_.classes) mustBe Seq(
        "govuk-input--width-2",
        "govuk-input--width-2 govuk-input--error",
        "govuk-input--width-4 govuk-input--error"
      )
    }

    "add the error class to all three inputs when the whole date is in error" in {
      val result = DateInputErrors.highlightErrors(
        cleanInput, formWithError(dateField, "ated.error.date.empty"), dateField
      )

      result.items.map(_.classes) mustBe Seq(
        "govuk-input--width-2 govuk-input--error",
        "govuk-input--width-2 govuk-input--error",
        "govuk-input--width-4 govuk-input--error"
      )
    }

    "leave the widths untouched and add no error class when the form is valid" in {
      val result = DateInputErrors.highlightErrors(cleanInput, testForm, dateField)

      result.items.map(_.classes) mustBe Seq(
        "govuk-input--width-2",
        "govuk-input--width-2",
        "govuk-input--width-4"
      )
    }

    "strip an error class from an input that is no longer in error" in {
      val alreadyErrored = dateInputWith(
        "govuk-input--width-2 govuk-input--error",
        "govuk-input--width-2 govuk-input--error",
        "govuk-input--width-4 govuk-input--error"
      )

      val result = DateInputErrors.highlightErrors(
        alreadyErrored, formWithError(s"$dateField.year", "ated.error.date.year.missing"), dateField
      )

      result.items.map(_.classes) mustBe Seq(
        "govuk-input--width-2",
        "govuk-input--width-2",
        "govuk-input--width-4 govuk-input--error"
      )
    }
  }
}
