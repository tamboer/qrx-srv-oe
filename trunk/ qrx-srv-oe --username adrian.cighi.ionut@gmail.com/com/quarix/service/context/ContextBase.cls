/*------------------------------------------------------------------------
   File        : ContextBase
   Purpose     :
   Syntax      :
   Description :
   Author(s)   : Marian
   Created     : Thu Jan 21 16:28:38 EET 2010
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
 ----------------------------------------------------------------------*/
routine-level on error undo, throw.

using com.quarix.service.session.iSession.

class com.quarix.service.context.ContextBase
   inherits   com.quarix.base.BaseObject
   implements com.quarix.service.context.iContext use-widget-pool:

   define public property    SessionPassThrough       as logical      no-undo   initial true
      get.
      protected set.

   define protected property contextSession           as iSession     no-undo
      get.
      set.

   constructor protected ContextBase ():
   end constructor.

   method public logical GetPassThrough ():
      return SessionPassThrough.
   end method.

   method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as character):
      define variable memptrValue as memptr no-undo.

      if GetValue(input sourceObject, input keyName, output memptrValue) then
      do:
         assign
            keyValue = get-string(memptrValue,1)
            set-size(memptrValue) = 0.

         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical GetValue (input keyName as character , output keyValue as character):

		case keyName:

			when 'SessionID':u
			then do:
				GetValue('', keyName, keyValue).

				/*dynamic-function('', input keyName, input keyValue).*/

				return true.
			end.

			when 'LanguageID':u
			then do:
				GetValue('', keyName, keyValue).

				/*dynamic-function('', input keyName, input keyValue).*/

				return true.
			end.

			when 'UsrID':u
			then do:
				GetValue('', keyName, keyValue).

				/*dynamic-function('', input keyName, input keyValue).*/

				return true.
			end.

			when 'Company':u
			then do:
				keyValue = dynamic-function('getgBedrijf') no-error.

				if Util:IsError()
				then GetValue('', keyName, keyValue).

				return true.
			end.

		end case. /* case keyName */

      	return GetValue('', keyName, keyValue).

   end method.

   method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as datetime):
      define variable memptrValue as memptr no-undo.

      if GetValue(input sourceObject, input keyName, output memptrValue) then
      do:
         assign
            keyValue  = datetime(date(get-short(memptrValue, 1), get-short(memptrValue, 17), get-short(memptrValue, 33)),
                                 get-long(memptrValue,49))
            set-size(memptrValue) = 0 .
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical GetValue (input keyName as character , output keyValue as datetime):
      return GetValue('', keyName, keyValue).
   end method.

   method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as datetime-tz):
      define variable memptrValue as memptr no-undo.

      if GetValue(input sourceObject, input keyName, output memptrValue) then
      do:
         assign
            keyValue  = datetime-tz(date(get-short(memptrValue, 1), get-short(memptrValue, 17), get-short(memptrValue, 33)),
                                    get-long(memptrValue, 49), get-long  (memptrValue, 81))
            set-size(memptrValue) = 0.
            return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical GetValue (input keyName as character , output keyValue as datetime-tz):
      return GetValue('', keyName, keyValue).
   end method.

   method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as rowid):
      define variable memptrValue as memptr no-undo.

      if GetValue(input sourceObject, input keyName, output memptrValue) then
      do:
         assign
            keyValue = to-rowid(get-string(memptrValue, 1))
            set-size(memptrValue) = 0.
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical GetValue (input keyName as character , output keyValue as rowid):
      return GetValue('', keyName, keyValue).
   end method.

   method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as memptr):

      set-size(keyValue) = 0.
      /* if pass-through we load data from session */
      if SessionPassThrough and valid-object(contextSession) then
         return contextSession:GetValue(sourceObject, keyName, output keyValue).
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical GetValue (input keyName as character , output keyValue as memptr):
      return GetValue('', keyName, keyValue).
   end method.

   method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as date):
      define variable memptrValue as memptr no-undo.

      if GetValue(input sourceObject, input keyName, output memptrValue) then
      do:
         assign
            keyValue  = date(get-short(memptrValue, 1), get-short(memptrValue, 17), get-short(memptrValue, 33))
            set-size(memptrValue) = 0 .
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical GetValue (input keyName as character , output keyValue as date):
      return GetValue('', keyName, keyValue).
   end method.

   method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as logical):
      define variable memptrValue as memptr no-undo.

      if GetValue(input sourceObject, input keyName, output memptrValue) then
      do:
         assign
            keyValue = get-byte(memptrValue,1) eq ascending('T':u)
            set-size(memptrValue) = 0.
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical GetValue (input keyName as character , output keyValue as logical):
      return GetValue('', keyName, keyValue).
   end method.

   method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as longchar):
      define variable memptrValue as memptr no-undo.

      if GetValue(input sourceObject, input keyName, output memptrValue) then
      do:
         assign
            keyValue = get-string(memptrValue,1)
            set-size(memptrValue) = 0 .
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical GetValue (input keyName as character , output keyValue as longchar):
      return GetValue('', keyName, keyValue).
   end method.

   method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as decimal):
      define variable memptrValue as memptr no-undo.

      if GetValue(input sourceObject, input keyName, output memptrValue) then
      do:
         assign
            keyValue = get-double(memptrValue,1)
            set-size(memptrValue) = 0 .
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical GetValue (input keyName as character , output keyValue as decimal):
      return GetValue('', keyName, keyValue).
   end method.

   method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as int64):
      define variable memptrValue as memptr no-undo.

      if GetValue(input sourceObject, input keyName, output memptrValue) then
      do:
         assign
            keyValue = get-int64(memptrValue,1)
            set-size(memptrValue) = 0 .
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical GetValue (input keyName as character , output keyValue as int64):
      return GetValue('', keyName, keyValue).
   end method.

   method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as integer):
      define variable memptrValue as memptr no-undo.

      if GetValue(input sourceObject, input keyName, output memptrValue) then
      do:
         assign
            keyValue = get-long(memptrValue,1)
            set-size(memptrValue) = 0 .
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical GetValue (input keyName as character , output keyValue as integer):
      return GetValue('', keyName, keyValue).
   end method.

   method public logical SetPassThrough (passThrough as logical):
      SessionPassThrough = passThrough.
      return true.
   end method.

   method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as character):
      define variable memptrValue as memptr no-undo.

      if length(keyValue) gt 0 then
      do:
         set-size(memptrValue)= length(keyValue,'raw') + 1.
         put-string(memptrValue,1) = keyValue.
      end.

      if SetValue(input sourceObject, input keyName, input memptrValue) then
      do:
         set-size(memptrValue) = 0.
         return true.
      end.

      set-size(memptrValue) = 0.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical SetValue (input keyName as character , input keyValue as character):

		case keyName:

			when 'SessionID':u
			then do:
				/*dynamic-function('', input keyName, input keyValue).*/
			end.

			when 'LanguageID':u
			then do:
				/*dynamic-function('', input keyName, input keyValue).*/
			end.

			when 'UsrID':u
			then do:
				/*dynamic-function('', input keyName, input keyValue).*/
			end.

			when 'Company':u
			then do:
				dynamic-function('setgBedrijf', input keyValue, input false) no-error.
			end.

		end case. /* case keyName */

      	return SetValue('', keyName, keyValue).

   end method.

   method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as datetime):
      define variable memptrValue as memptr no-undo.

      set-size(memptrValue) = 82.
      put-short(memptrValue,1)  = month(keyValue) .
      put-short(memptrValue,17) = day(keyValue)   .
      put-short(memptrValue,33) = year(keyValue)  .
      put-long(memptrValue,49)  = mtime(keyValue) .

      if SetValue(input sourceObject, input keyName, input memptrValue) then
      do:
         set-size(memptrValue) = 0.
         return true.
      end.

      set-size(memptrValue) = 0.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical SetValue (input keyName as character , input keyValue as datetime):
      return SetValue('', keyName, keyValue).
   end method.

   method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as datetime-tz):
      define variable memptrValue as memptr no-undo.

      set-size(memptrValue) = 113.
      put-short(memptrValue,1)  = month(keyValue)    .
      put-short(memptrValue,17) = day(keyValue)      .
      put-short(memptrValue,33) = year(keyValue)     .
      put-long(memptrValue,49)  = mtime(keyValue)    .
      put-long(memptrValue,81)  = timezone(keyValue) .

      if SetValue(input sourceObject, input keyName, input memptrValue) then
      do:
         set-size(memptrValue) = 0.
         return true.
      end.

      set-size(memptrValue) = 0.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical SetValue (input keyName as character , input keyValue as datetime-tz):
      return SetValue('', keyName, keyValue).
   end method.

   method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as rowid):
      define variable memptrValue as memptr no-undo.

      set-size(memptrValue) = length(string(keyValue),'raw') + 1.
      put-string(memptrValue,1) = string(keyValue) .

      if SetValue(input sourceObject, input keyName, input memptrValue) then
      do:
         set-size(memptrValue) = 0.
         return true.
      end.

      set-size(memptrValue) = 0.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical SetValue (input keyName as character , input keyValue as rowid):
      return SetValue('', keyName, keyValue).
   end method.

   method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as memptr):
      /* if pass-through we save data in session */
      if SessionPassThrough and valid-object(contextSession) then
         return contextSession:SetValue(sourceObject, keyName, keyValue).
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical SetValue (input keyName as character , input keyValue as memptr):
      return SetValue('', keyName, keyValue).
   end method.

   method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as date):
      define variable memptrValue as memptr no-undo.

      set-size(memptrValue) = 50.
      put-short(memptrValue,1)  = month(keyValue) .
      put-short(memptrValue,17) = day(keyValue)   .
      put-short(memptrValue,33) = year(keyValue)  .

      if SetValue(input sourceObject, input keyName, input memptrValue) then
      do:
         set-size(memptrValue) = 0.
         return true.
      end.

      set-size(memptrValue) = 0.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical SetValue (input keyName as character , input keyValue as date):
      return SetValue('', keyName, keyValue).
   end method.

   method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as logical):
      define variable memptrValue as memptr no-undo.

      set-size(memptrValue) = 2.
      put-byte(memptrValue,1) = asc(string(keyValue,'T/F':u)) .

      if SetValue(input sourceObject, input keyName, input memptrValue) then
      do:
         set-size(memptrValue) = 0.
         return true.
      end.

      set-size(memptrValue) = 0.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical SetValue (input keyName as character , input keyValue as logical):
      return SetValue('', keyName, keyValue).
   end method.

   method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as longchar):
      define variable memptrValue as memptr no-undo.

      set-size(memptrValue) = length(keyValue,'raw') + 1.
      put-string(memptrValue,1) = keyValue .

      if SetValue(input sourceObject, input keyName, input memptrValue) then
      do:
         set-size(memptrValue) = 0.
         return true.
      end.

      set-size(memptrValue) = 0.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical SetValue (input keyName as character , input keyValue as longchar):
      return SetValue('', keyName, keyValue).
   end method.

   method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as decimal):
      define variable memptrValue as memptr no-undo.

      set-size(memptrValue) = 64.
      put-double(memptrValue,1) = keyValue .

      if SetValue(input sourceObject, input keyName, input memptrValue) then
      do:
         set-size(memptrValue) = 0.
         return true.
      end.

      set-size(memptrValue) = 0.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical SetValue (input keyName as character , input keyValue as decimal):
      return SetValue('', keyName, keyValue).
   end method.

   method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as int64):
      define variable memptrValue as memptr no-undo.

      set-size(memptrValue) = 64.
      put-int64(memptrValue,1) = keyValue.

      if SetValue(input sourceObject, input keyName, input memptrValue) then
      do:
         set-size(memptrValue) = 0.
         return true.
      end.

      set-size(memptrValue) = 0.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical SetValue (input keyName as character , input keyValue as int64):
      return SetValue('', keyName, keyValue).
   end method.

   method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as integer):
      define variable memptrValue as memptr no-undo.

      set-size(memptrValue) = 32.
      put-long(memptrValue,1) = keyValue.

      if SetValue(input sourceObject, input keyName, input memptrValue) then
      do:
         set-size(memptrValue) = 0.
         return true.
      end.

      set-size(memptrValue) = 0.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical SetValue (input keyName as character , input keyValue as integer):
      return SetValue('', keyName, keyValue).
   end method.

   method public logical SaveContext (input sessionManager as com.quarix.service.session.iSession):
      return false.
   end method.

   method public logical LoadContext (input sessionManager as com.quarix.service.session.iSession):

      if valid-object(sessionManager) then
      do:
         Purge().
         /* if pass-through we don't actually load session data, it will be loaded on demand */
         if SessionPassThrough then
         do:
            contextSession = sessionManager.
            return true.
         end.
         return sessionManager:LoadContext(this-object).
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public void Purge ():
   end method.

   method public void Purge (input sourceObject as character):
   end method.

   method public logical RemoveValue (input sourceObject as character, input keyName as character):
      return SetValue (sourceObject, keyName, '').
   end method.

   method public logical RemoveValue (input keyName as character):
      return RemoveValue ('', keyName).
   end method.

end class.
