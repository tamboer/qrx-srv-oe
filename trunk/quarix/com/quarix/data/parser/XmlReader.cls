/*------------------------------------------------------------------------
   File        : XmlReader
   Purpose     :
   Syntax      :
   Description : Common xml parser implementation using SAX Reader
   Author(s)   : Marian
   Created     : Tue Dec 23 16:31:40 EET 2008
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

using com.quarix.system.FileSystem.
using com.quarix.data.parser.XmlReader.

class com.quarix.data.parser.XmlReader
   inherits com.quarix.base.BaseObject
   implements com.quarix.base.iSingleton use-widget-pool:

   define private variable SaxReader         as handle   no-undo.
   define private variable ElementAttributes as handle   no-undo.
   define private variable HelperHandle      as handle   no-undo.
   define private variable saxInputSource    as longchar no-undo.

   define private property FileSys             as FileSystem no-undo
      get:
         if not valid-object(FileSys) then
            FileSys = cast(GetInstance('com.quarix.system.FileSystem':u), 'com.quarix.system.FileSystem':u).
         return FileSys.
      end get.
      set.

   define public property ElementContentLength as int64     no-undo
      get.
      protected set.

   define public property ElementNumAttributes as integer   no-undo
      get:
         return ElementAttributes:num-items.
      end get.
      protected set.

   define private temp-table ttOutStream no-undo
      field streamIndex   as int64
      field contentLength as int64
      field contentBody   as blob
      index PK_ttOutStream is primary streamIndex.


   &if keyword-all('static':u) ne ? &then
   define private static variable xmlReader as XmlReader no-undo.

   constructor private XmlReader():
      initialize().

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end constructor.

   method public static XmlReader GetInstance():
      if not valid-object(xmlReader) then
         xmlReader = new XmlReader().
      return xmlReader.
   end method.

   &else
   constructor public XmlReader():
      do on error undo, throw:
         run com/quarix/base/enforceSingleton.p (this-object).
      end.

      initialize().

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end constructor.
   &endif

   method private void initialize ( ):
      create sax-reader     SaxReader         .
      create sax-attributes ElementAttributes .

      if valid-object(FileSys) and
         FileSys:IsFile('com/quarix/data/parser/XmlReaderHelper.r':u) or
         FileSys:IsFile('com/quarix/data/parser/XmlReaderHelper.p':u) then
         run com/quarix/data/parser/XmlReaderHelper.p  persistent set HelperHandle (input this-object).
      SaxReader:handler = HelperHandle.
   end method.



   /*------------------------------------------------------------------------------
       Purpose:
       Notes:
   ------------------------------------------------------------------------------*/
   destructor public XmlReader ( ):
      delete object    SaxReader         .
      delete object    ElementAttributes .
      delete procedure HelperHandle      .
      UnloadInstance(FileSys).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end destructor.

   method public final logical SetSource(fileName as character):
      if not valid-handle(SaxReader)  or
         not valid-object(FileSys)    or
         not FileSys:IsFile(fileName) or
         not FileSys:CanRead(fileName) then return false.
      return SaxReader:set-input-source('file':u, fileName).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public final logical SetSource (mpContent as memptr):
      define variable lcContent as longchar no-undo.

      copy-lob from mpContent to lcContent.
      return SetSource(lcContent).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public final logical SetSource (lcContent as longchar):
      if not valid-handle(SaxReader) or
         length(lcContent) eq 0 then return false.
      saxInputSource = lcContent.

      return SaxReader:set-input-source('longchar':u, saxInputSource).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public final void Reset ():
      assign
         ElementContentLength = 0
         ElementNumAttributes = 0.
      empty temp-table ttOutStream.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method public final logical ParseNext ():
      if not valid-handle(SaxReader) or
         SaxReader:parse-status eq sax-complete then
         return false.
      if SaxReader:parse-status eq sax-running then
         SaxReader:sax-parse-next() .
      else
         SaxReader:sax-parse-first() .
      return SaxReader:parse-status ne sax-parser-error.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public final logical Parse ():
      if not valid-handle(SaxReader) then
         return false.
      SaxReader:sax-parse().
      if not Util:IsError() and SaxReader:parse-status ne sax-parser-error then
         return true.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public final character GetElementAttribute (attrName as character):
      if valid-handle(ElementAttributes) then
         return ElementAttributes:get-value-by-qname (attrName).
      return ?.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method public final character GetElementAttribute (attrIndex as integer):
      if attrIndex gt 0 and attrIndex le ElementNumAttributes
         and valid-handle(ElementAttributes) then
         return ElementAttributes:get-value-by-index (attrIndex).
      return ?.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method public final void AppendElementContent (content as memptr, contentLen as integer):
      create ttOutStream.
      assign
         ttOutStream.streamIndex   = ElementContentLength
         ttOutStream.contentLength = contentLen
         ElementContentLength      = ElementContentLength + contentLen
         ttOutStream.contentBody   = content .

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end.

   method public void ElementOpen (URI as character, LocalName as character, QName as character):
   end.

   method public void ElementClose (URI as character, LocalName as character, QName as character):
   end.

   method public final void StartElement (URI as character, LocalName as character, QName as character, Attributes as handle):
      ElementContentLength = 0.
      empty temp-table ttOutStream.
      ElementAttributes:copy-sax-attributes(Attributes) .
      ElementOpen(URI, LocalName, QName).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end.

   method public final void EndElement (URI as character, LocalName as character, QName as character):
      ElementClose(URI, LocalName, QName).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end.

   method public void ElementContent (mpResponse as memptr):

      assign
         set-size(mpResponse) = 0
         set-size(mpResponse) = ElementContentLength.
      for each ttOutStream
         on error undo, throw:
         copy-lob ttOutStream.contentBody for ttOutStream.contentLength to mpResponse overlay at ttOutStream.streamIndex + 1 no-convert.
      end.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

end class.
