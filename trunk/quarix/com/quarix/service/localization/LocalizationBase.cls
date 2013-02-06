/*---------------------------------------------------------------------
File        : LocalizationBase.cls
Purpose     :

Syntax      :

Description :

Author(s)   : Marian
Created     :
Notes       :
    License     :
    This file is part of the QRX-SRV-OE software framework.
    Copyright (C) 2011, SC Yonder SRL (http://www.tss-yonder.com)

    The QRX-SRV-OE software framework is free software; you can redistribute
    it and/or modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either version 2.1
    of the License, or (at your option) any later version.

    The QRX-SRV-OE software framework is distributed in the hope that it will
    be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
    General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with the QRX-SRV-OE software framework; if not, write to the Free
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
    02110-1301  USA or on the internet at the following address:
    http://www.gnu.org/licenses/lgpl-2.1.txt
---------------------------------------------------------------------*/
routine-level on error undo, throw.

class com.quarix.service.localization.LocalizationBase
   inherits com.quarix.base.BaseObject
   implements com.quarix.service.localization.iLocalization use-widget-pool:


   define public property Country             as character   no-undo
      get.
      private set.

   define public property Language            as character   no-undo
      get.
      private set.

   define public property DateFormat          as character   no-undo
      get.
      private set.

   define public property Locale              as character   no-undo
      get.
      private set.

   define public property LogicalFormat       as character   no-undo   initial 'true/false':u
      get.
      private set.

   define public property NumericFormat        as character   no-undo
      get.
      private set.

   define public property ResourceBundle       as character   no-undo
      get.
      private set.

   constructor protected LocalizationBase ():
   end constructor.

   method public final character GetCountry  ():
      return Country.
   end method.

   method public final character GetLanguage ():
      return Language.
   end method.

   method public final character GetLocale   ():
      return Locale.
   end method.

   method public final logical   SetLocale   (localeCode as character):
      if localeCode eq Locale then
         return true.

      if num-entries(localeCode, '_':u) ne 2 or
         not SetCountry  (entry(1, localeCode, '_':u)) or
         not SetLanguage (entry(2, localeCode, '_':u)) then
         return false.

      assign
         Locale   = lower(localeCode)
         Country  = entry(1, localeCode, '_':u)
         Language = entry(2, localeCode, '_':u).
      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public final character GetDateFormat ():
      return DateFormat.
   end method.

   method public final logical   SetDateFormat (newFormat as character):
      if Util:IsEmpty(newFormat)           or
         (session:date-format eq newFormat and
          DateFormat eq newFormat) then
         return true.

      session:date-format = newFormat.
      DateFormat = lower(newFormat).
      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public final character GetLogicalFormat ():
      return LogicalFormat.
   end method.

   method public final logical   SetLogicalFormat (newFormat as character):
      if newFormat eq ? or num-entries(newFormat, '~/':u) ne 2 then
         return false.
      LogicalFormat = newFormat.
      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public final character GetNumericFormat ():
      return NumericFormat.
   end method.

   method public final logical   SetNumericFormat (newFormat as character):
      if NumericFormat eq newFormat then
         return true.

      session:numeric-format = newFormat.
      NumericFormat = lower(newFormat).
      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public final character LocaleValue (input initValue as character, input dataType as character):
      return LocaleValue (initValue, dataType, ?).
   end method.

   method public final character LocaleValue (input initValue as character, input dataType as character, input dataFormat as character):
      define variable intVal   as int64         no-undo.
      define variable boolVal  as logical       no-undo.
      define variable decVal   as decimal       no-undo.
      define variable dateVal  as date          no-undo.
      define variable dtVal    as datetime      no-undo.
      define variable dtzVal   as datetime-tz   no-undo.

      if initValue eq ? then
         return ?.

      case dataType:
         when 'integer':u or
         when 'int64':u then
            do:
               intVal = int64(initValue).
               return LocaleValue(intVal, dataFormat).
            end.
         when 'decimal':u then
            do:
               decVal = decimal(initValue) .
               return LocaleValue(decVal, dataFormat).
            end.
         when 'logical':u then
            do:
               boolVal = logical(initValue) .
               if dataFormat eq ? or dataFormat eq 'yes/no':u then
                  return LocaleValue(boolVal).
               return LocaleValue(string(boolVal, dataFormat)).
            end.
         when 'date':u then
            do:
               dateVal = date(initValue) .
               return LocaleValue(dateVal, dataFormat).
            end.
         when 'datetime':u then
            do:
               dtVal = datetime(initValue) .
               return LocaleValue(dtVal, dataFormat).
            end.
         when 'datetime-tz':u then
            do:
               dtzVal = datetime-tz(initValue) .
               return LocaleValue(dtzVal, dataFormat).
            end.
         otherwise
         return LocaleValue(initValue).
      end case.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method public character LocaleValue (input keyName as character):
   end method.

   method public final character LocaleValue (input boolValue as logical):
      return trim(string(boolValue, LogicalFormat)).
   end method.

   method public final character LocaleValue (input numericValue as decimal):
      return LocaleValue (numericValue, ?).
   end method.

   method public final character LocaleValue (input dateValue as date):
      return LocaleValue (dateValue, ?).
   end method.

   method public final character LocaleValue (input dateValue as datetime):
      return LocaleValue (dateValue, ?).
   end method.


   method public final character LocaleValue (input dateValue as datetime-tz):
      return LocaleValue (dateValue, ?).
   end method.

   method public final character LocaleValue (input numericValue as integer, input dataFormat as character):
      define variable stRes as character no-undo.
      define variable stNumericFormat as character no-undo.

      if numericValue eq ? then
         return ?.

      if dataFormat eq ? then
         return string(numericValue).

      if session:numeric-format eq NumericFormat then
         return trim(string(numericValue, dataFormat)).

      assign
         stNumericFormat        = session:numeric-format
         session:numeric-format = NumericFormat
         stRes                  = trim(string(numericValue, dataFormat))
         session:numeric-format = stNumericFormat.

      return stRes.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method public final character LocaleValue (input numericValue as int64, input dataFormat as character):
      define variable stRes           as character no-undo.
      define variable stNumericFormat as character no-undo.

      if numericValue eq ? then
         return ?.

      if dataFormat eq ? then
         return string(numericValue).

      if session:numeric-format eq NumericFormat then
         return trim(string(numericValue, dataFormat)).

      assign
         stNumericFormat        = session:numeric-format
         session:numeric-format = NumericFormat
         stRes                  = trim(string(numericValue, dataFormat))
         session:numeric-format = stNumericFormat.

      return stRes.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.




   method public final character LocaleValue (input numericValue as decimal, input dataFormat as character):
      define variable stRes           as character no-undo.
      define variable stNumericFormat as character no-undo.

      if numericValue eq ? then
         return ?.

      if session:numeric-format eq NumericFormat then do on error undo, return ?:
         stRes = if dataFormat eq ? then string(numericValue) else string(numericValue, dataFormat).
      end.
      else
         assign
            stNumericFormat        = session:numeric-format
            session:numeric-format = NumericFormat
            stRes                  = if dataFormat eq ? then string(numericValue) else string(numericValue, dataFormat)
            session:numeric-format = stNumericFormat.

      return trim(stRes).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method public final character LocaleValue (input dateValue as date, input dataFormat as character):

      define variable stRes        as character no-undo.
      define variable stDateFormat as character no-undo.

      if dateValue eq ? then
         return ?.

      if session:date-format eq DateFormat then do on error undo, return ?:
         stRes = if dataFormat eq ? then string(dateValue) else string(dateValue, dataFormat).
      end.
      else
         assign
            stDateFormat        = session:date-format
            session:date-format = DateFormat
            stRes               = if dataFormat eq ? then string(dateValue) else string(dateValue, dataFormat)
            session:date-format = stDateFormat.

      return trim(stRes).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method public final character LocaleValue (input dateValue as datetime, input dataFormat as character):

      define variable stRes        as character no-undo.
      define variable stDateFormat as character no-undo.

      if dateValue eq ? then
         return ?.

      if session:date-format eq DateFormat then do on error undo, return ?:
         stRes = if dataFormat eq ? then string(dateValue) else string(dateValue, dataFormat).
      end.
      else
         assign
            stDateFormat        = session:date-format
            session:date-format = DateFormat
            stRes               = if dataFormat eq ? then string(dateValue) else string(dateValue, dataFormat)
            session:date-format = stDateFormat.

      return trim(stRes).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.

   end method.


   method public final character LocaleValue (input dateValue as datetime-tz, input dataFormat as character):
      define variable stRes        as character no-undo.
      define variable stDateFormat as character no-undo.

      if dateValue eq ? then
         return ?.

      if session:date-format eq DateFormat then do on error undo, return ?:
         stRes = if dataFormat eq ? then string(dateValue) else string(dateValue, dataFormat).
      end.
      else
         assign
            stDateFormat        = session:date-format
            session:date-format = DateFormat
            stRes               = if dataFormat eq ? then string(dateValue) else string(dateValue, dataFormat)
            session:date-format = stDateFormat.

      return trim(stRes).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.



   method public final character GetFormat (input dataType  as character, input dataFormat as character):
      case dataType:
         when 'logical':u then
            do:
               if dataFormat eq ? or dataFormat eq 'yes~/no':u or dataFormat eq LogicalFormat then
                  return LogicalFormat.
               else
                  return substitute('&1~/&2':u,
                     LocaleValue(entry(1, dataFormat, '~/':u)),
                     if num-entries(dataFormat, '~/':u) eq 2 then LocaleValue(entry(2, dataFormat, '~/':u)) else '':u).
            end.
         when 'integer':u or
         when 'decimal':u or
         when 'int64':u then
            do:
               if NumericFormat eq 'european':u then
                  return replace(replace(replace(dataFormat, '.':u, chr(1)), ',':u, '.':u), chr(1), ',':u).
            end.
         when 'date':u or
         when 'datetime':u or
         when 'datetime-tz':u then
            return lower(dataFormat).
      end case.
      return dataFormat.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method public final logical OpenResource (input resourceCode as character):
      if UseResource(resourceCode) then
      do:
         ResourceBundle = resourceCode.
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public final void CloseResource (input resourceCode as character):
      if resourceCode eq ResourceBundle then
         assign
            ResourceBundle = ?
            Locale         = ?
            Language       = ?
            Country        = ?
            DateFormat     = ?
            NumericFormat  = ?
            LogicalFormat  = ?.
      UnloadResource(resourceCode).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method public void Purge ():
   end method.

   method public void Purge (input resourceCode as character):
   end method.

   method protected logical UseResource (input resourceCode as character):
      return true.
   end method.

   method protected void UnloadResource (input resourceCode as character):
      Purge(resourceCode).
   end method.

   method protected logical SetCountry  (countryCode as character):
      return true.
   end method.

   method protected logical SetLanguage  (languageCode as character):
      return true.
   end method.

end class.


