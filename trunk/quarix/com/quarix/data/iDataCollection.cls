
/*------------------------------------------------------------------------
    File        : iDataCollection
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marian
    Created     : Mon Jan 19 11:04:40 EET 2009
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

  interface com.quarix.data.iDataCollection:

     method public logical    SetFilter (fieldName as character, operName as character, fieldValue as character).

     method public void       RemoveFilter (fieldName as character).

     method public void       ClearFilters ().

     method public logical    SetSort (fieldName as character).

     method public logical    SetSort (fieldName as character, descendingSort as logical).

     method public void       RemoveSort (fieldName as character).

     method public void       ClearSort ().

     method public logical    SetBatchSize (bufferName as character, batchSize as integer).

     method public logical    SetBatchSize (batchSize as integer).

     method public integer    GetBatchSize (bufferName as character).

     method public logical    LoadData ().

     method public logical    SaveData ().

     method public character  GetPhysicalFieldName (logicalName as character).

     method public void       SetDataSource (pcTable as character, pcBuffer as character).

     method public void       SetDataSource (pcTable as character, pcBuffer as character, pcKeyFields as character).

     method public void       SetDataSource (pcTable     as character, pcBuffer as character,
                                             pcKeyFields as character, pcWhere  as character).

     method public void       SetDataSource (pcTable     as character, pcBuffer as character,
                                             pcKeyFields as character, pcWhere  as character, pcFieldsMap as character).

	 method public void       AddDataSource (pcTable as character, pcBuffer as character).

     method public void       AddDataSource (pcTable as character, pcBuffer as character, pcKeyFields as character).

     method public void       AddDataSource (pcTable     as character, pcBuffer as character,
                                             pcKeyFields as character, pcWhere  as character).

     method public void       AddDataSource (pcTable     as character, pcBuffer as character,
                                             pcKeyFields as character, pcWhere  as character, pcFieldsMap as character).

  end interface.
