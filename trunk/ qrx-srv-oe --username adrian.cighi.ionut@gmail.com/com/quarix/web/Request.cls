 /*------------------------------------------------------------------------
    File        : Request
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : dragos
    Created     : Tue Dec 09 09:47:02 EET 2008
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

using com.quarix.web.Request.

class  com.quarix.web.Request
   inherits com.quarix.base.BaseObject
   implements com.quarix.base.iSingleton
   use-widget-pool final:

   &global-define res-group-input      1
   &global-define res-group-header     2
   &global-define res-group-cgi        3
   &global-define res-group-session    4

   define private temp-table fileItem no-undo
       field fieldName         as character   xml-node-type 'attribute':u
       field fileName          as character   xml-node-type 'attribute':u
       field mimeType          as character   xml-node-type 'attribute':u
       field fileSize          as int64       xml-node-type 'attribute':u
       field fileBody          as clob
       index pk is primary unique fieldName.

   define private dataset fileUpload for fileItem.

   define public property ContentType           as character   no-undo
      get.
      private set.

   define public property NumUploadFiles        as integer     no-undo
      get.
      private set.

   define public property RequestMethod         as character   no-undo
      get.
      private set.

   define public property RequestStream         as memptr      no-undo
      get.
      private set.

   define public property DocumentRoot          as character   no-undo
      get.
      private set.

   define public property ApplicationName       as character   no-undo
      get.
      private set.

   define public property ApplicationPath       as character   no-undo
      get:
         return substitute('&1~/&2':u, DocumentRoot, ApplicationName).
      end get.
      private set.

   define public property ModuleName            as character   no-undo
      get.
      private set.

   define public property ModuleMethod          as character   no-undo
      get.
      private set.

   define public property AjaxRequest           as logical     no-undo
      get.
      private set.

   define private variable fileUploadXsd        as character   no-undo.

   define private variable requestTableHandle   as handle      no-undo.

   &if keyword-all('static':u) ne ? &then
   define private static variable webRequest    as Request no-undo.

   constructor private Request():
   end constructor.

   method public static Request GetInstance():
      if not valid-object(webRequest) then
         webRequest = new Request().
      return webRequest.
   end method.

   &else

   constructor public Request():
      do on error undo, return error:
         run com/quarix/base/enforceSingleton.p (this-object).
      end.
   end constructor.
   &endif

   method public void Initialize (table-handle httRequest, reqStream as memptr):
      define variable pathInfo    as character no-undo.
      define variable numEntries  as integer   no-undo.

      requestTableHandle = httRequest:default-buffer-handle.
      RequestStream      = reqStream.

      assign
         RequestMethod   = GetCgi('REQUEST_METHOD':u)
         ContentType     = GetCgi('CONTENT_TYPE':u)
         AjaxRequest     = GetHttpHeader('X-Requested-With':u) eq 'XMLHttpRequest':u
         pathInfo        = left-trim(GetCgi('PATH_INFO':u), '~/')
         numEntries      = num-entries(pathInfo, '~/':u)
         ApplicationName = entry(1, pathInfo, '~/':u)
         ModuleMethod    = entry(numEntries, pathInfo, '~/':u)
         entry(1, pathInfo, '~/':u)          = ''
         entry(numEntries, pathInfo, '~/':u) = ''
         ModuleName      = trim(pathInfo, '~/':u)
         DocumentRoot    = substitute('&1&2':u,
                                       Util:Nvl(GetCgi('context_path':u), ''),
                                       Util:Nvl(GetCgi('servlet_path':u), '')).

      /* if multi-part attachement request */
      if Util:Nvl(GetCgi('CHECK_ATTACHMENTS'), 'false') eq 'true' then
      do:
         if fileUploadXsd eq '':u then do:
            file-info:file-name = 'com/quarix/web/xsd/FileUpload.xsd':u.
            if file-information:full-pathname ne ? then
               fileUploadXsd = file-information:full-pathname.
         end.
         if fileUploadXsd ne '':u then do:
            dataset fileUpload:read-xml('memptr':u, reqStream, 'empty':u, fileUploadXsd, false, ?, 'ignore':u).
            for each fileItem
                on error undo, throw:
               if Util:IsEmpty(fileItem.fileName) or fileItem.fileBody eq ? then
                  delete fileItem.
               else
                  NumUploadFiles = NumUploadFiles + 1.
            end.
            if NumUploadFiles gt 0 then
               set-size(reqStream) = 0.
         end.
      end.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end method.

   method public  character  GetCgi(input stName as character ):
      return getFieldValue ({&res-group-cgi}, stName).
   end method.

   method public  character GetCookie(input stName as character ):
      return getFieldValue ({&res-group-session}, stName).
   end method.

   method public  character GetHttpHeader(input stName as character ):
      return getFieldValue ({&res-group-header}, stName).
   end method.

   method public  character GetVariable (input stName as character ):
      return getFieldValue ({&res-group-input}, stName).
   end method.

   method public  memptr GetBlobVariable (input stName as character ):
      return getFieldBlob ({&res-group-input}, stName).
   end method.

   method public  character GetUploadField (input fieldNum as integer ):
      define buffer fileItem for fileItem.

      define query qryFile for fileItem scrolling.

      if fieldNum le 0 or fieldNum gt NumUploadFiles then
         return ?.

      open query qryFile for each fileItem.
      if fieldNum gt 1 then
         reposition qryFile to row fieldNum.

      get next qryFile.
      if available(fileItem) then do:
         return fileItem.fieldName.
      end.

      return ?.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return ?.
      end catch.
      finally:
         close query qryFile.
      end finally.
   end method.

   method public  memptr GetUploadFile (input fieldName as character ):
      define variable mpFile as memptr    no-undo.
      define variable lcFile as longchar  no-undo.

      define buffer fileItem for fileItem.

      set-size(mpFile) = 0.
      for each fileItem
         where fileItem.fieldName eq fieldName
            on error undo, throw:
         assign
            lcFile = fileItem.fileBody
            mpFile = base64-decode(trim(lcFile, '~n~r':u)).
         leave.
      end.
      return mpFile.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return ?.
      end catch.
   end method.

   method public  int64 GetUploadFileSize (input fieldName as character ):
      define buffer fileItem for fileItem.

      for each fileItem
         where fileItem.fieldName eq fieldName:
         return fileItem.fileSize.
      end.
      return ?.
   end method.

   method public  character GetUploadFileName (input fieldName as character ):
      define buffer fileItem for fileItem.

      for each fileItem
         where fileItem.fieldName eq fieldName:
         return fileItem.fileName.
      end.
      return ?.
   end method.

   method public  character GetUploadFileContentType (input fieldName as character ):
      define buffer fileItem for fileItem.

      for each fileItem
         where fileItem.fieldName eq fieldName:
         return fileItem.mimeType.
      end.
      return ?.
   end method.

   method private character getFieldValue(input lType as integer , input stName  as character ):
      define variable varValue as memptr    no-undo.
      define variable retValue as character no-undo initial ?.

      varValue = getFieldBlob (lType, stName).
      if varValue eq ? then
         return ?.

      if get-size(varValue) gt 0 then do:
         retValue = get-string(varValue, 1).
         set-size(varValue) = 0.
      end.

      return retValue.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return ?.
      end catch.
   end method.

   method private memptr  getFieldBlob(input lType as integer , input stName  as character ):
      define variable retValue as memptr no-undo.

      set-size(retValue) = 0.
      if valid-handle(requestTableHandle) then do:
         requestTableHandle:find-first(substitute('where fieldType eq &1 and fieldName eq &2':u, lType, quoter(stName))) no-error.
         if requestTableHandle:available then
            copy-lob requestTableHandle::fieldValue to retValue.
      end.
      return retValue.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return ?.
      end catch.
   end method.

   method public void Reset():
   end method.

   method public void Empty ():
      empty temp-table fileItem.

      assign
         RequestMethod   = ?
         ContentType     = ?
         AjaxRequest     = ?
         ApplicationName = ?
         ModuleMethod    = ?
         ModuleName      = ?
         DocumentRoot    = ?
         NumUploadFiles  = 0.

     catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
     end catch.
   end method.
end class.
