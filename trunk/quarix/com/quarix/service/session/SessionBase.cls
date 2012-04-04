/*------------------------------------------------------------------------
   File        : SessionBase
   Purpose     :
   Syntax      :
   Description :
   Author(s)   : Andriuhan
   Created     : Mon Dec 22 13:45:21 EET 2008
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

class com.quarix.service.session.SessionBase
   inherits   com.quarix.base.BaseObject
   implements com.quarix.service.session.iSession use-widget-pool:

   define public property ID                      as character  no-undo initial ?
      get.
      protected set.

   define public property MaxInactivityInterval   as integer    no-undo initial 1800
      get.
      protected set.

   define public property LastAccessTime          as datetime   no-undo
      get.
      protected set.

   define public property StartTime               as datetime   no-undo
      get.
      protected set.

   constructor protected SessionBase ():
   end constructor.

   method public final character GetID ():
      return ID.
   end method.

   method public final integer GetMaxInactivityInterval ():
      return MaxInactivityInterval.
   end method.

   method public final logical GetValue (input sourceObject as character, input keyName as character , output keyValue as character):
      define variable memptrValue as memptr no-undo.

      keyValue = ?.
      if GetValue(input sourceObject, input keyName, output memptrValue) and get-size(memptrValue) gt 0 then
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

   method public final logical GetValue (input keyName as character , output keyValue as character):
      return GetValue ('', keyName, output keyValue).
   end method.

   method public final logical GetValue (input sourceObject as character, input keyName as character , output keyValue as datetime):
      define variable memptrValue as memptr no-undo.

      keyValue = ?.

      if GetValue(input sourceObject, input keyName, output memptrValue) and get-size(memptrValue) gt 0 then
      do:
         assign
            keyValue = datetime(date(get-short(memptrValue, 1), get-short(memptrValue, 17), get-short(memptrValue, 33)),
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

   method public final logical GetValue (input keyName as character , output keyValue as datetime):
      return GetValue ('', keyName, output keyValue).
   end method.

   method public final logical GetValue (input sourceObject as character, input keyName as character , output keyValue as datetime-tz):
      define variable memptrValue as memptr no-undo.

      keyValue = ?.

      if GetValue(input sourceObject, input keyName, output memptrValue) and get-size(memptrValue) gt 0 then
      do:
         assign
            keyValue = datetime-tz(date(get-short(memptrValue, 1), get-short(memptrValue, 17), get-short(memptrValue, 33)),
                                    get-long(memptrValue, 49), get-long  (memptrValue, 81))
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

   method public final logical GetValue (input keyName as character , output keyValue as datetime-tz):
      return GetValue ('', keyName, output keyValue).
   end method.

   method public final logical GetValue (input sourceObject as character, input keyName as character , output keyValue as rowid):
      define variable memptrValue as memptr no-undo.

      keyValue = ?.

      if GetValue(input sourceObject, input keyName, output memptrValue) and get-size(memptrValue) gt 0 then
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

   method public final logical GetValue (input keyName as character , output keyValue as rowid):
      return GetValue ('', keyName, output keyValue).
   end method.

   method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as memptr):

   end method.

   method public final logical GetValue (input keyName as character , output keyValue as memptr):
      return GetValue ('', keyName, output keyValue).
   end method.

   method public final logical GetValue (input sourceObject as character, input keyName as character , output keyValue as date):
      define variable memptrValue as memptr no-undo.

      keyValue = ?.

      if GetValue(input sourceObject, input keyName, output memptrValue) and get-size(memptrValue) gt 0 then
      do:
         assign
            keyValue = date(get-short(memptrValue, 1), get-short(memptrValue, 17), get-short(memptrValue, 33))
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

   method public final logical GetValue (input keyName as character , output keyValue as date):
      return GetValue ('', keyName, output keyValue).
   end method.

   method public final logical GetValue (input sourceObject as character, input keyName as character , output keyValue as logical):
      define variable memptrValue as memptr no-undo.

      keyValue = ?.

      if GetValue(input sourceObject, input keyName, output memptrValue) and get-size(memptrValue) gt 0 then
      do:
         assign
            keyValue = logical(get-short(memptrValue,1))
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

   method public final logical GetValue (input keyName as character , output keyValue as logical):
      return GetValue ('', keyName, output keyValue).
   end method.

   method public final logical GetValue (input sourceObject as character, input keyName as character , output keyValue as longchar):
      define variable memptrValue as memptr no-undo.

      keyValue = ?.

      if GetValue(input sourceObject, input keyName, output memptrValue) and get-size(memptrValue) gt 0 then
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

   method public final logical GetValue (input keyName as character , output keyValue as longchar):
      return GetValue ('', keyName, output keyValue).
   end method.

   method public final logical GetValue (input sourceObject as character, input keyName as character , output keyValue as decimal):
      define variable memptrValue as memptr  no-undo.

      keyValue = ?.

      if GetValue(input sourceObject, input keyName, output memptrValue) and get-size(memptrValue) gt 0 then
      do:
         assign
            keyValue = get-double(memptrValue,1)
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

   method public final logical GetValue (input keyName as character , output keyValue as decimal):
      return GetValue ('', keyName, output keyValue).
   end method.

   method public final logical GetValue (input sourceObject as character, input keyName as character , output keyValue as int64):
      define variable memptrValue as memptr  no-undo.

      keyValue = ?.

      if GetValue(input sourceObject, input keyName, output memptrValue) and get-size(memptrValue) gt 0 then
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

   method public final logical GetValue (input keyName as character , output keyValue as int64):
      return GetValue ('', keyName, output keyValue).
   end method.


   method public final logical GetValue (input sourceObject as character, input keyName as character , output keyValue as integer):
      define variable memptrValue as memptr  no-undo.

      keyValue = ?.

      if GetValue(input sourceObject, input keyName, output memptrValue) and get-size(memptrValue) gt 0 then
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

   method public final logical GetValue (input keyName as character , output keyValue as integer):
      return GetValue ('', keyName, output keyValue).
   end method.

   method public final logical SetValue (input sourceObject as character, input keyName as character , input keyValue as character):
      define variable memptrValue as memptr  no-undo.

      if length(keyValue) gt 0 then
      do:
         set-size(memptrValue)= length(keyValue,'raw') + 1.
         put-string(memptrValue,1) = keyValue .
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

   method public final logical SetValue (input keyName as character , input keyValue as character):
      return SetValue('', keyName, keyValue).
   end method.

   method public final logical SetValue (input sourceObject as character, input keyName as character , input keyValue as datetime):
      define variable memptrValue as memptr  no-undo.

      set-size(memptrValue) = 112.
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

   method public final logical SetValue (input keyName as character , input keyValue as datetime):
      return SetValue('', keyName, keyValue).
   end method.

   method public final logical SetValue (input sourceObject as character, input keyName as character , input keyValue as datetime-tz):
      define variable memptrValue as memptr  no-undo.

      set-size(memptrValue) = 112.
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

   method public final logical SetValue (input keyName as character , input keyValue as datetime-tz):
      return SetValue('', keyName, keyValue).
   end method.

   method public final logical SetValue (input sourceObject as character, input keyName as character , input keyValue as rowid):
      define variable memptrValue as memptr  no-undo.

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

   method public final logical SetValue (input keyName as character , input keyValue as rowid):
      return SetValue('', keyName, keyValue).
   end method.

   method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as memptr):

   end method.

   method public final logical SetValue (input keyName as character , input keyValue as memptr):
      return SetValue('', keyName, keyValue).
   end method.

   method public final logical SetValue (input sourceObject as character, input keyName as character , input keyValue as date):
      define variable memptrValue as memptr  no-undo.

      set-size(memptrValue) = 112.
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

   method public final logical SetValue (input keyName as character , input keyValue as date):
      return SetValue('', keyName, keyValue).
   end method.


   method public final logical SetValue (input sourceObject as character, input keyName as character , input keyValue as logical):
      define variable memptrValue as memptr  no-undo.

      set-size(memptrValue) = 16.
      put-short(memptrValue,1) = integer(keyValue) .

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

   method public final logical SetValue (input keyName as character , input keyValue as logical):
      return SetValue('', keyName, keyValue).
   end method.

   method public final logical SetValue (input sourceObject as character, input keyName as character , input keyValue as longchar):
      define variable memptrValue as memptr  no-undo.

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

   method public final logical SetValue (input keyName as character , input keyValue as longchar):
      return SetValue('', keyName, keyValue).
   end method.

   method public final logical SetValue (input sourceObject as character, input keyName as character , input keyValue as decimal):
      define variable memptrValue as memptr  no-undo.

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

   method public final logical SetValue (input keyName as character , input keyValue as decimal):
      return SetValue('', keyName, keyValue).
   end method.

   method public final logical SetValue (input sourceObject as character, input keyName as character , input keyValue as int64):
      define variable memptrValue as memptr  no-undo.

      set-size(memptrValue) = 64.
      put-int64(memptrValue,1) = keyValue .

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

   method public final logical SetValue (input keyName as character , input keyValue as int64):
      return SetValue('', keyName, keyValue).
   end method.

   method public final logical SetValue (input sourceObject as character, input keyName as character , input keyValue as integer):
      define variable memptrValue as memptr  no-undo.

      set-size(memptrValue) = 32.
      put-long(memptrValue,1) = keyValue .

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

   method public final logical SetValue (input keyName as character , input keyValue as integer):
      return SetValue('', keyName, keyValue).
   end method.

   method public final logical RemoveValue (input sourceObject as character, input keyName as character):
      return SetValue (sourceObject, keyName, '').
   end method.

   method public final logical RemoveValue (input keyName as character):
      return RemoveValue ('', keyName).
   end method.

   method public final void Destroy ():
      Destroy(ID).
      ID = ?.
   end method.

   method public void Destroy (sessId as character):
   end method.

   method public logical LoadContext (input sessContext as com.quarix.service.context.iContext):
   end method.

   method public final logical SaveContext (input sessContext as com.quarix.service.context.iContext):
      if valid-object(sessContext) then
         return sessContext:SaveContext(this-object).
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public final void Start ():
      start (?, ?).
   end method.

   method public final void Start (sessId as character):
      start(sessId, ?).
   end method.

   method public void Start (sessId as character, sessTimeOut as integer):
      assign
         LastAccessTime = dtutil:sysNow()
         ID             = Util:Nvl(sessId, guid).

      if sessTimeOut ne ? then
         MaxInactivityInterval = sessTimeOut.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end.
end class.
