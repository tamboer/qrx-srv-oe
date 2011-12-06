/*---------------------------------------------------------------------
File        : SaxLogger.cls
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

class com.quarix.service.logging.SaxLogger
   inherits com.quarix.service.logging.BaseLogger
   implements com.quarix.base.iSingleton use-widget-pool:


   define private variable numMessages    as integer no-undo.

   define public property  SaxWriter      as handle  no-undo
      get.
      set (saxHandle as handle):
         if valid-handle(saxHandle) and saxHandle:type eq 'sax-writer':u then
         do:
            if saxHandle eq SaxWriter then
               return.
            CloseLog().
            SaxWriter = saxHandle.
         end.
      end set.


   constructor public SaxLogger():
   end constructor.

   constructor public SaxLogger(saxHandle as handle):
      SaxWriter = saxHandle.
   end constructor.

   method override public logical LogError(input errorLevel as integer, input  errorCode as integer,
      input errorText   as character, input errorParam as character,
      input errorObject as character, input errorTime  as datetime ):

      if IsOpen and valid-handle(SaxWriter) then
      do:
         saxWriter:start-element('message':u).
         saxWriter:insert-attribute('type':u, string(errorLevel)).
         if not Util:IsEmpty(errorParam) then
            saxWriter:insert-attribute('column':u, errorParam).
         saxWriter:insert-attribute('msg':u, errorText).
         saxWriter:end-element('message':u).
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

    /*roxanam : for the new error template*/
   method override public logical LogError
      (input errorLevel as integer, input errorCode as integer, input errorText as character,
      input errorFile as character, input errorMethod as character,
      input errorLine as integer, input errorTime as datetime):

      if IsOpen and valid-handle(SaxWriter) then
      do:
         saxWriter:start-element('message':u).
         saxWriter:insert-attribute('type':u, string(errorLevel)).
         if not Util:IsEmpty(errorFile) then
            saxWriter:insert-attribute('location':u, substitute('&1 &2 &3':u, Util:Nvl(errorFile, '':u), Util:Nvl(errorMethod, '':u), Util:Nvl(string(errorLine), '':u))).
         saxWriter:insert-attribute('msg':u, errorText).
         saxWriter:end-element('message':u).
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method override public logical OpenLog():
      if not IsOpen and valid-handle(SaxWriter) then
      do:
         IsOpen = true.
         SaxWriter:start-element('messages':u).
      end.
      return IsOpen.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method override public logical CloseLog():
      if IsOpen and valid-handle(SaxWriter) then
         SaxWriter:end-element('messages':u).
      IsOpen = false.
      return true.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method public void Reset():
   end method.

end class.


