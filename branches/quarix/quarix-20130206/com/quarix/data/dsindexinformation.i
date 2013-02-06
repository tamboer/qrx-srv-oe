
/*------------------------------------------------------------------------
    File        : dsindexinformation.i
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Adam
    Created     : Wed Aug 24 19:19:54 EEST 2011
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

	define temp-table ttIndexInformation
		field TtableHandle		as handle
		field BufferHandle		as handle
		field TableName			as character
		field BufferName		as character
		field IndexId			as integer
		field IndexName			as character
		field UniqueIndex		as logical
		field PrimaryIndex		as logical
		field WordIndex			as logical
		index PK_idx			is primary unique IndexId
		index idxBuffer			is unique BufferHandle IndexName.

	define temp-table ttIndexFields
		field IndexId			as integer
		field FieldName			as character
		field SortDescending	as logical
		index PK_idx is primary unique IndexId FieldName.

	define dataset dsIndexInformation for ttIndexInformation, ttIndexFields
		data-relation drIndexInformation for ttIndexInformation, ttIndexFields relation-fields(IndexId, IndexId).


