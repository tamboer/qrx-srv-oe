
/*------------------------------------------------------------------------
    File        : XmlReaderHelper.p
    Purpose     : Handle call-back methods of SAX Reader

    Syntax      :

    Description : Xml Reader Helper

    Author(s)   : Marian
    Created     : Wed Dec 24 08:57:07 EET 2008
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

define input parameter XmlReader as com.quarix.data.parser.XmlReader no-undo.

procedure endElement:
   define input parameter URI        as character.
   define input parameter LocalName  as character.
   define input parameter QName      as character.

   if valid-object(XmlReader) then
      XmlReader:EndElement(URI, LocalName, QName).
end procedure.

procedure startElement:
   define input parameter URI        as character.
   define input parameter LocalName  as character.
   define input parameter QName      as character.
   define input parameter Attributes as handle.

   if valid-object(XmlReader) then
      XmlReader:StartElement(URI, LocalName, QName, Attributes).
end procedure.

procedure characters:
   define input parameter content    as memptr.
   define input parameter contentLen as integer.

   if valid-object(XmlReader) then
      XmlReader:AppendElementContent(content, contentLen).
end procedure.
