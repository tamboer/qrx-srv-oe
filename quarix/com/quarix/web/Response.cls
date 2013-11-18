 /*------------------------------------------------------------------------
    File        : Response
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : dragos
    Created     : Thu Dec 04 14:06:44 EET 2008
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

&global-define res-group-output     1
&global-define res-group-header     2
&global-define res-group-cookie     3
&global-define res-group-java       4

&global-define res-status-code      'STATUS-CODE':u

using com.quarix.web.Request.
using com.quarix.web.Response.
using com.quarix.data.parser.JsonWriter.
using com.quarix.system.FileSystem.

class com.quarix.web.Response
   inherits com.quarix.base.BaseObject
   implements com.quarix.base.iSingleton
   use-widget-pool final:

   define public property ContentType       as character   no-undo
      get.
      set (newType as character):
         ContentType = newType.
         SetHttpHeader('content-type':u, ContentType).
      end set.

   define public property ResponseType      as character   no-undo
      get.
      set (newType as character):
         ResponseType = newType.
         SetJavaHeader('response-type':u, ResponseType).
      end set.

   define public property ContentSize       as int64       no-undo
      get.
      private set.

   define public property RedirectURL      as character   no-undo
      get.
      set (redirectAddress as character):
         RedirectURL = redirectAddress.
         redirectToURL().
      end set.

   define public property EnableCache      as logical     no-undo
      get.
      set (useCache as logical):
         if setEnableCache(useCache) then
            EnableCache = useCache.

         if EnableCache eq false then
         do:
            SetHttpHeader('cache-control':u, 'no-cache':u).
            SetHttpHeader('pragma':u, 'no-cache':u).
         end.
      end set.

   define public property RESPONSE_DIRECT  as character   no-undo   initial 'direct':u
      get.

   define public property RESPONSE_XSLT    as character   no-undo   initial 'xsltprocess':u
      get.

   define public property RESPONSE_REPORT  as character   no-undo   initial 'report':u
      get.

   define public property RESPONSE_BASE64  as character   no-undo   initial 'base64':u
      get.

   define private variable request_        as Request     no-undo.

   define private property Request         as Request     no-undo
      get:
         if not valid-object(request_) then
            request_ = cast(GetInstance('com.quarix.web.Request':u), 'Request':u).
         return request_.
      end get.

   define private variable jsonWriter_        as JsonWriter     no-undo.

   define private property jsonWriter         as JsonWriter     no-undo
      get:
         if not valid-object(jsonWriter_ ) then
            jsonWriter_ = cast(GetInstance('com.quarix.data.parser.JsonWriter':u), 'JsonWriter':u).
         return jsonWriter_ .
      end get.

   define private variable fileSystem_        as FileSystem     no-undo.

   define private property fileSystem         as FileSystem     no-undo
      get:
         if not valid-object(fileSystem_) then
            fileSystem_ = cast(GetInstance('com.quarix.system.FileSystem':u), 'FileSystem':u).
         return fileSystem_.
      end get.

   define private variable responseTableHandle  as handle     no-undo.

   define private variable noCache              as logical    no-undo   initial ?.

   define private variable nullMem              as memptr     no-undo.

   define private variable saxWriter_           as handle     no-undo.

   define private property saxWriter            as handle     no-undo
      get:
         if not valid-handle(saxWriter_) then
         do:
            create sax-writer saxWriter_.
            assign
               saxWriter_:strict   = false
               saxWriter_:fragment = true
               saxWriter_:encoding = 'utf-8'.
         end.
         return saxWriter_.
      end get.


   &if keyword-all('static':u) ne ? &then
   define private static variable webResponse    as Response no-undo.

   constructor private Response():

   end constructor.

   method public static Response GetInstance():
      if not valid-object(webResponse) then
         webResponse = new Response().
      return webResponse.
   end method.

   &else

   constructor public Response():
      do on error undo, return error:
         run com/quarix/base/enforceSingleton.p (this-object).
      end.

   end constructor.
   &endif

   destructor public Response ():
      set-size(nullMem) = 0.
      delete object saxWriter_.
      UnloadInstance(jsonWriter_).
      UnloadInstance(fileSystem_).
      UnloadInstance(request_).
   end destructor.

   method public logical Out (input  mpOut as memptr ) :
      define variable len   as integer  no-undo.
      define variable clOut as longchar no-undo.

      copy-lob from mpOut to clOut.
      do while len lt length(clOut)
          on error undo, throw:
         if not Out(substr(clOut, len + 1, 20000)) then
            return false.
         len = len + 20000.
      end.
      return true.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method public logical Out (input  stOut as character ):

      define variable lEmpty as logical no-undo.

      lEmpty = Util:IsEmpty(stOut) no-error.

      if lEmpty then
         return true.

      if not saxWriter_:write-fragment(stOut) then
         return false.
      ContentSize = ContentSize + length(stOut, 'raw':u).
      return true.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.


   method public logical Out (input  stOut as longchar) :

      define variable lEmpty as logical no-undo.

      lEmpty = Util:IsEmpty(stOut) no-error.

      if lEmpty then
         return true.

      if not saxWriter_:write-fragment(stOut) then
         return false.
      ContentSize = ContentSize + length(stOut, 'raw':u).
      return true.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method public void Reset ():
      set-size(nullMem) = 0.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end method.

   method public void Initialize (table-handle ttResponse, rspStream as memptr):

      responseTableHandle = ttResponse:default-buffer-handle.

      assign
         ResponseType = RESPONSE_XSLT
         ContentType  = 'text/html':u
         EnableCache  = false
         noCache      = ?
         RedirectURL  = ?.

      setStreamDestination (rspStream).
      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end method.

   method public logical SetJavaHeader (input  stName as character, input  stValue as character ):
      return setFieldValue ({&res-group-java}, caps(stName), stValue).
   end method.

   method public logical SetHttpHeader(input stName as character, input stValue as character ):
      return setFieldValue ({&res-group-header},
         right-trim(stName,' ~t:':u),
         stValue).

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method public logical SetHttpStatusCode(input lStatusCode as integer ,input  stMessage as character ):
      return setFieldValue ({&res-group-header},
         {&res-status-code},
         substitute('HTTP/1.0 &1 &2':u, lStatusCode, stMessage)).

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method public logical StreamFile(input stFile as character, input  stMimeType as character, input stDisposition as character ):
      define variable mpFile    as memptr   no-undo.
      define variable encFile   as longchar no-undo.

      if Util:IsEmpty(stFile) then
         return false.

      if FileSystem:IsFile(stFile) and FileSystem:CanRead(stFile) then
      do:
         if stDisposition = 'inline':u then
            SetHttpHeader('Content-Disposition':u, 'inline':u).
         else
            SetHttpHeader('Content-Disposition':u, substitute('attachment; filename=&1':u, FileSystem:GetName(stFile))).

         copy-lob from file FileSystem:GetFullPath(stFile) to mpFile.

         if get-size(mpFile) eq 0 then
            return false.

         assign
            encFile       = base64-encode(mpFile)
            ResponseType  = RESPONSE_BASE64
            ContentType   = Util:Nvl(stMimeType, 'application/x-unknown':u)
            stDisposition = Util:Nvl(stDisposition, 'inline':u).

         Out(encFile).
         set-size(mpFile) = 0.
         return true.
      end.

      return false.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method public logical StreamFile(input stFile as character,input  stMimeType as character ):
      return streamFile(stFile, stMimeType, '').
   end method.

   method public logical StreamFile(input stFile as character ):
      return streamFile(stFile, '', '').
   end method.

   method public logical StreamFile(input stFile as character, input fileContent as memptr, input  stMimeType as character, input stDisposition as character ):
      define variable encFile   as longchar no-undo.

      if fileContent eq ? then
         return false.

      if get-size(fileContent) eq 0 then
         return false.

      if stDisposition = 'inline':u then
         SetHttpHeader('Content-Disposition':u, 'inline':u).
      else
         SetHttpHeader('Content-Disposition':u, substitute('attachment; filename=&1':u, stFile)).

      encFile = base64-encode(fileContent).

      assign
         ResponseType  = RESPONSE_BASE64
         ContentType   = Util:Nvl(stMimeType, 'application/x-unknown':u)
         stDisposition = Util:Nvl(stDisposition, 'inline':u).
      Out(encFile).
      return true.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method public logical StreamFile(input stFile as character, input fileContent as memptr, input  stMimeType as character ):
      return streamFile(stFile, fileContent, stMimeType, ?).
   end method.

   method public logical StreamFile(input stFile as character, input fileContent as memptr):
      return streamFile(stFile, fileContent, ?, ?).
   end method.

   method public logical SetCookie
      (input stName as character, input stValue as character,
      input dtExpire as datetime-tz, input stPath as character,
      input stDomain as character, input fSecure as logical):

      define variable dzNow as datetime-tz no-undo.

      if Util:IsEmpty(stPath) and valid-object(Request) then
         stPath = Request:ApplicationPath.

      assign dzNow = DtUtilManager:sysNow().

      stValue  = substitute('&1&2&3&4&5':u,
         Util:UrlEncode(stValue,'cookie':u),
         (if dtExpire eq ? then '':u else substitute('~; Max-Age=&1':u, round((dtExpire - dzNow) / 1000, 0))),
         (if Util:isEmpty(stPath)   then '':u else substitute('~; Path=&1':u, stPath)),
         (if Util:isEmpty(stDomain) then '':u else substitute('~; Domain=':u, stDomain)),
         (if fSecure then '~; Secure':u else '':u)).
      return setFieldValue ({&res-group-cookie}, stName, stValue).


      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method public logical SetCookie
      (input stName as character, input stValue as character,
      input dtExpire as datetime-tz, input stPath as character, input stDomain as character):
      return SetCookie(stName, stValue, dtExpire, stPath, stDomain, ?).
   end method.

   method public logical SetCookie(input stName as character, input stValue as character, input dtExpire as datetime-tz):
      return SetCookie(stName, stValue, dtExpire, ?, ?, ?).
   end method.

   method public logical SetCookie(input stName as character, input stValue as character):
      return SetCookie(stName, stValue, ?).
   end method.

   method private logical setEnableCache (input useCache as logical):
      if noCache eq ? or useCache eq false then
      do:
         noCache = not useCache.
         return true.
      end.
      return false.
   end method.

   method public void Close ():
      setStreamDestination (nullMem).

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end method.

   method public void Empty ():
      ContentSize = 0.
      emptyStream().
      if valid-handle(responseTableHandle) then
         responseTableHandle:empty-temp-table().

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end method.

   method private logical setFieldValue(input lType as integer ,input  stName as character ,input  stValue as character ):

      if valid-handle(responseTableHandle) then
      do transaction
          on error undo, throw:
         responseTableHandle:find-first(substitute('where fieldType eq &1 and fieldName eq &2':u, lType, quoter(stName))) no-error.
         if responseTableHandle:available then
         do:
            if Util:IsEmpty(stValue) then
               responseTableHandle:buffer-delete().
            else
               responseTableHandle::fieldValue = stValue.
            return true.
         end.

         if Util:IsEmpty(stValue) then
            return true.

         responseTableHandle:buffer-create().
         assign
            responseTableHandle::fieldType  = lType
            responseTableHandle::fieldName  = stName
            responseTableHandle::fieldValue = stValue .
         return true.
      end.
      return false.
   end method.

   method private void redirectToURL ():
      define variable redirectStr as memptr no-undo.

      if Util:IsEmpty(RedirectURL) then return.

      Empty().

      ResponseType = RESPONSE_DIRECT.

      if valid-object(jsonWriter) then
      do:
         ContentType  = 'application/json':u.

         jsonWriter:WriteMetaInfo = true.
         jsonWriter:SetDestination(redirectStr).
         jsonWriter:OpenStream().

         jsonWriter:StartElement('').

         jsonWriter:WriteValue('redirectURL':u, RedirectURL).
         jsonWriter:EndElement('').
         jsonWriter:CloseStream().
         Out(redirectStr).
      end.
      else
      do:
         SetHttpStatusCode(302, 'Redirect':u).
         SetHttpHeader('Location':u, RedirectURL).
      end.

   end method.

   method private void setStreamDestination (rspStream as memptr):
      if not valid-handle(saxWriter) then
         return.

      if saxWriter_:write-status ne sax-write-idle and
         saxWriter_:write-status ne sax-write-complete then
         saxWriter_:end-document().

      saxWriter_:set-output-destination('memptr':u, rspStream).

      if rspStream eq ? or rspStream eq nullMem then
         return.

      saxWriter_:start-document().
   end method.

   method private void emptyStream ():
      if not valid-handle(saxWriter_) then
         return.

      if saxWriter_:write-status ne sax-write-idle and
         saxWriter_:write-status ne sax-write-complete then
         saxWriter_:end-document().
      saxWriter_:start-document().

   end method.


end class.


