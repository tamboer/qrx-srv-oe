/*---------------------------------------------------------------------
File        : iLocalization.cls
Purpose     :

Syntax      :

Description :

Author(s)   : Marian
Created     : generated 17/12/08
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

interface  com.quarix.service.localization.iLocalization:

    method public logical   OpenResource  (resourceName as character).
    method public void      CloseResource (resourceName as character).
    method public void      Purge ().
    method public void      Purge (resourceName as character).

    method public character LocaleValue (input keyName as character).
    method public character LocaleValue (input initValue as character, input dataType as character, input dataFormat as character).
    method public character LocaleValue (input keyName as character, input dataType as character).

    method public character LocaleValue (input boolValue as logical).
    method public character LocaleValue (input numericValue as decimal).
    method public character LocaleValue (input dateValue as date).
    method public character LocaleValue (input dateValue as datetime).
    method public character LocaleValue (input dateValue as datetime-tz).

    method public character LocaleValue (input numericValue as integer, input dataFormat as character).
    method public character LocaleValue (input numericValue as int64, input dataFormat as character).
    method public character LocaleValue (input numericValue as decimal, input dataFormat as character).
    method public character LocaleValue (input dateValue as date, input dataFormat as character).
    method public character LocaleValue (input dateValue as datetime, input dataFormat as character).
    method public character LocaleValue (input dateValue as datetime-tz, input dataFormat as character).

    method public character GetFormat (input dataType  as character, input dataFormat as character).

    method public character GetCountry  ().
    method public character GetLanguage ().

    method public character GetLocale   ().
    method public logical   SetLocale   (localeCode as character).

    method public character GetDateFormat ().
    method public logical   SetDateFormat (dateFormat as character).

    method public character GetLogicalFormat ().
    method public logical   SetLogicalFormat (logicalFormat as character).

    method public character GetNumericFormat ().
    method public logical   SetNumericFormat (numericFormat as character).
end interface.


