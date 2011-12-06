 /*------------------------------------------------------------------------
    File        : WebObject
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marian
    Created     : Tue Feb 03 11:11:00 EET 2009
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

using com.quarix.web.*.

class com.quarix.web.WebObject
   inherits com.quarix.base.BaseObject
   implements iWebObject use-widget-pool:

   define protected property Request            as Request     no-undo
      get.
      private set.

   define protected property Response           as Response    no-undo
      get.
      private set.

   define protected property EnableCache        as logical     no-undo
      get.
      set.

   /* abstract class */
   constructor protected WebObject ():
   end constructor.

   method public final logical HandleRequest (actionName as character, webRequest as Request, webResponse as Response):
      if not valid-object(webRequest) or
         not valid-object(webResponse) then
         return false.

      assign
         Request              = webRequest
         Response             = webResponse
         Response:EnableCache = EnableCache.

      return handleRequest ().

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method private logical handleRequest ():
      if not BeforePaint() or
         not HandleRequestPaint() then
         return false.
      AfterPaint().
      return true.
   end method.

   method protected logical HandleRequestPaint ():
      ThrowError(100, 'msg_err_method_not_implemented':u, ?, ?).
      return true.
   end method.

   method protected logical BeforePaint():
      return true.
   end method.


   method protected void AfterPaint():
   end method.

   method protected final logical DataObject (objectName as character):
      return InsertWebPart (objectName).
   end method.

   /* insert web-part - 'include' equivalent - default action name is 'paint' */
   method protected final logical InsertWebPart (objectName as character):
      return InsertWebPart (objectName, 'paint':u).
   end method.

   method protected final logical InsertWebPart (objectName as character, actionName as character):
      define variable webObj  as Progress.Lang.Object  no-undo.
      define variable retVal  as logical               no-undo.

      webObj = GetInstance(objectName).

      if not valid-object(webObj) then
         return false.

      if type-of(webObj, 'com.quarix.web.iWebObject':u) then do
         on error undo, throw
         on quit  undo, leave
         on stop  undo, leave:
         retVal = cast(webObj, 'com.quarix.web.iWebObject':u):HandleRequest(actionName, Request, Response).
      end.

      UnloadInstance(webObj).
      return retVal.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

end class.
