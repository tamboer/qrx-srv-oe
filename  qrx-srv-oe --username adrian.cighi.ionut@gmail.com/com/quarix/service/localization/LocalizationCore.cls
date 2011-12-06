/*---------------------------------------------------------------------
File        : LocalizationCore.cls
Purpose     :

Syntax      :

Description :

Author(s)   : Marian
Created     : Fri Jan 09 08:16:03 EET 2009
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

using com.quarix.service.localization.LocalizationCore.

class com.quarix.service.localization.LocalizationCore
   inherits com.quarix.service.localization.LocalizationBase
   implements com.quarix.base.iSingleton use-widget-pool:


    {com/quarix/service/localization/dsLocalization.i &scope=protected}

   define private variable ResourceBundleId    as integer    no-undo.
   define private variable LanguageId          as integer    no-undo.
   define private variable AppId               as character  no-undo.

   define private variable LastLangId          as integer   initial 0 no-undo.
   define private variable LastBundleId        as integer   initial 0 no-undo.

   define private temp-table ttCountry no-undo
      field countryCode   as character
      field langCode      as character
      field dateFormat    as character
      field numericFormat as character
      field logicalFormat as character
      index PK_ttCountry is primary unique countryCode.

    &if keyword-all('static':u) ne ? &then
   define private static variable localizationCore    as LocalizationCore no-undo.

   constructor private LocalizationCore():
   end constructor.

   method public static LocalizationCore GetInstance():
      if not valid-object(localizationCore) then
         localizationCore = new LocalizationCore().
      return localizationCore.
   end method.
   &else
   constructor public LocalizationCore():
      do on error undo, return error:
         run com/quarix/base/enforceSingleton.p (this-object).
      end.
   end constructor.
   &endif

   method override protected logical UseResource (input newResource as character):
   define buffer core_app for core_app.

      if Util:IsEmpty(newResource) then
      do:
         assign
            ResourceBundleId = ?
            AppId            = ?.
         return true.
      end.

      for each ttResourceBundle
         where ttResourceBundle.bundleName eq newResource
            on error undo, throw:
         assign
            ResourceBundleId = ttResourceBundle.bundleId
            AppId            = ttResourceBundle.externalId.
         return true.
      end.

      for each core_app no-lock
         where core_app.app_name eq newResource
            on error undo, throw:
         create ttResourceBundle.
         assign
            LastBundleId                = LastBundleId + 1
            ttResourceBundle.bundleId   = LastBundleId
            ttResourceBundle.bundleName = newResource
            ttResourceBundle.externalId = core_app.app_id
            ResourceBundleId            = ttResourceBundle.bundleId
            AppId                       = ttResourceBundle.externalId.
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method override public character LocaleValue (input keyName as character):
   define buffer ttResource for ttResource.

      for each ttResource
         where ttResource.langId  eq LanguageId
         and ttResource.keyName eq keyName:
         return ttResource.keyString.
      end.

      return loadResource(LanguageId, keyName).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method override protected void UnloadResource (input resourceCode as character):
   define buffer ttResourceBundle  for ttResourceBundle.

      for each ttResourceBundle
         where ttResourceBundle.bundleName eq resourceCode
            on error undo, throw:
         Purge(resourceCode).
         delete ttResourceBundle.
      end.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method override public void Purge ():
       dataset dsLocalization:empty-dataset().

       catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
       end catch.
   end method.

   method override public void Purge (input resourceCode as character):
   define buffer ttResourceBundle  for ttResourceBundle.
   define buffer ttLanguage        for ttLanguage.
   define buffer ttResource        for ttResource.

      for each ttResourceBundle
         where ttResourceBundle.bundleName eq resourceCode
            on error undo, throw:
         for each ttLanguage
            where ttLanguage.bundleId eq ttResourceBundle.bundleId
               on error undo, throw:

            for each ttResource
               where ttResource.langId eq ttLanguage.langId
                  on error undo, throw:
               delete ttResource.
            end.
            delete ttLanguage.
         end.
      end.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method private character loadResource (input langId as integer, input keyName as character):
      define variable keyString as  character no-undo initial ?.

      define buffer loc_label   for loc_label.
      define buffer ttResource  for ttResource.
      define buffer ttLanguage  for ttLanguage.

      for each ttLanguage
         where ttLanguage.langId eq langId
            on error undo, throw:
         for each loc_label no-lock
            where loc_label.app_id    eq AppId
            and loc_label.lang_code eq ttLanguage.langCode
            and loc_label.lbl_code  eq keyName
               on error undo, throw:
            keyString = loc_label.lbl_text.
         end.
         if keyString eq ? and ttLanguage.parentId ne ? then
            keyString = loadResource(ttLanguage.parentId, keyName).

         create ttResource.
         assign
            keyString            = Util:Nvl(keyString, keyName)
            ttResource.langId    = langId
            ttResource.keyName   = keyName
            ttResource.keyString = keyString.
         return keyString.
      end.

      return keyName.
   end method.

   method private integer loadLanguage (languageCode as character):
   define buffer ttLanguage   for ttLanguage.
   define buffer loc_lang     for loc_lang.
   define buffer loc_app_lang for loc_app_lang.

      if Util:IsEmpty(languageCode) then return ?.

      for each ttLanguage
         where ttLanguage.langCode eq languageCode:
         return ttLanguage.langId.
      end.

      for each loc_app_lang no-lock
         where loc_app_lang.app_id    eq AppId
         and loc_app_lang.lang_code eq languageCode,
         each  loc_lang no-lock
         where loc_lang.lang_code  eq loc_app_lang.lang_code
            on error undo, throw:

         create ttLanguage.
         assign
            LastLangId             = LastLangId + 1
            ttLanguage.parentId    = loadLanguage(loc_lang.parent_lang)
            ttLanguage.bundleId    = ResourceBundleId
            ttLanguage.langId      = LastLangId
            ttLanguage.langCode    = languageCode.
         return ttLanguage.langId.
      end.
      return ?.
   end method.

   method protected override logical SetLanguage (input languageCode as character):
      define variable langId as integer no-undo.

      langId = loadLanguage(languageCode).
      if langId eq ? then
         return false.

      LanguageId  = langId.
      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method protected override logical SetCountry (input countryCode as character):
   define buffer loc_country  for loc_country.

      for each ttCountry
         where ttCountry.countryCode eq countryCode
            on error undo, throw:

         SetDateFormat(ttCountry.dateFormat).
         SetLogicalFormat(ttCountry.logicalFormat).
         SetNumericFormat(ttCountry.numericFormat).
         SetLanguage(ttCountry.langCode).
         return true.
      end.

      for each loc_country no-lock
         where loc_country.country_code eq countryCode
            on error undo, throw:
         create ttCountry.
         assign
            ttCountry.countryCode   = countryCode
            ttCountry.langCode      = loc_country.default_lang
            ttCountry.dateFormat    = loc_country.date_format
            ttCountry.logicalFormat = loc_country.logical_format
            ttCountry.numericFormat = loc_country.numeric_format.

         SetDateFormat(ttCountry.dateFormat).
         SetLogicalFormat(ttCountry.logicalFormat).
         SetNumericFormat(ttCountry.numericFormat).
         SetLanguage(ttCountry.langCode).
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public final void Reset ():
      if valid-object(Application) and OpenResource(Application:Name) then
      do:
         SetLocale        (Application:Locale).
         SetDateFormat    (Application:DateFormat).
         SetNumericFormat (Application:NumericFormat).
         SetLogicalFormat (Application:LogicalFormat).
      end.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.
end class.


