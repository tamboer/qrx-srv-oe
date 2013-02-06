
/*------------------------------------------------------------------------
    File        : iDataAccess
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Andriuhan
    Created     : Mon Nov 29 10:49:23 EET 2010
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

   using Progress.Lang.*.
   using com.quarix.base.Collection.
   using com.quarix.data.*.

   interface com.quarix.data.iDataAccess:

      method public logical		AddDataSource (pcTable as character, pcBuffer as character).

      method public logical		AddDataSource (pcTable as character, pcBuffer as character, pcKeyFields as character).

      method public logical		AddDataSource (pcTable as character, pcBuffer as character, pcKeyFields as character, pcWhere as character).

      method public logical		AddDataSource (pcTable as character, pcBuffer as character, pcKeyFields as character, pcWhere as character, pcFieldsMap as character).

      method public logical     AddDataSource (pcTable as character, pcBuffer as character, pcKeyFields as character, pcWhere as character, pcFieldsMap as character, excludeFields as character).

      method public logical     AddDataSource (pcTable as character, pcBuffer as character, pcKeyFields as character, pcWhere as character, pcFieldsMap as character, excludeFields as character, includeFields as character).

      method public logical     AddDataSource (pcTable as character, pcBuffer as character, pcKeyFields as character, pcWhere as character, pcFieldsMap as character, excludeFields as character, includeFields as character, joinClause as character).

      method public logical     AddDataSource (pcTable as character, pcBuffer as character, pcKeyFields as character, pcWhere as character, pcFieldsMap as character, excludeFields as character, includeFields as character, joinClause as character, joinType as logical).

      method public logical     AddDataSource (pcTable as character, phBuffer as handle,    pcKeyFields as character, pcWhere as character, pcFieldsMap as character, excludeFields as character, includeFields as character, joinClause as character).

      method public logical     AddDataSource (pcTable as character, phBuffer as handle,    pcKeyFields as character, pcWhere as character, pcFieldsMap as character, excludeFields as character, includeFields as character, joinClause as character, joinType as logical).

      method public logical     AttachDataSource ().

      method public logical     AddJoinClause  (tableName as character, bufferName as character, joinClause as character).

      method public logical     AddWhereClause (tableName as character, bufferName as character, whereClause as character).

      method public Collection  AutocompleteKey (tableName as character, keyField as character, keyValue as character).

      method public Collection  AutocompleteKey (tableName as character, keyField as character, keyValue as character, filterXml as memptr).

      method public character   GetKeyValue     (tableName as character, keyField as character, keyValue as character, lookupField as character, filterXml as memptr).

      method public logical     CheckMandatory  (hBuf as handle, pcField as character).

      method public void        DetachDataSource ().

      method public void        DisableDebugQuery().

      method public void        EnableDebugQuery().

      method public handle      GetIdListTmpHandle().

      method public character   GetFullFieldName     (fieldName    as character).

      method public character   GetLogicalFieldName  (physicalName as character).

      method public character	GetPhysicalFieldName (logicalName  as character).

      method public character   GetQueryName().

      method public logical     BeforeRowFill   (hBuf    as handle).

      method public logical     BeforeRowDelete (hBuffBi as handle, hDbBuff as handle).

      method public logical     BeforeRowSave   (hBuff   as handle, hBuffBi as handle, hDbBuff as handle).

      method public logical     AfterRowFill    (hBuf    as handle).

      method public logical     AfterRowDelete  (hBuffBi as handle).

      method public logical     AfterRowSave    (hBuff   as handle, hDbBuff as handle).

      method public logical     BeforeLoadData ().

      method public logical     AfterLoadData  ().

      method public logical		LoadData ().

      method public logical     BeforeSaveData ().

      method public logical     AfterSaveData  ().

      method public logical		SaveData ().

      method public void        LocalizeDataset (dataset-handle dsHandle).

      method public void        SetDataModel    (dtModel as DataModel).

      method public logical		SetDataSource (pcTable as character, pcBuffer as character).

      method public logical		SetDataSource (pcTable as character, pcBuffer as character, pcKeyFields as character).

      method public logical		SetDataSource (pcTable as character, pcBuffer as character, pcKeyFields as character, pcWhere as character).

      method public logical		SetDataSource (pcTable as character, pcBuffer as character, pcKeyFields as character, pcWhere as character, pcFieldsMap as character).

      method public logical     SetDataSource (pcTable as character, pcBuffer as character, pcKeyFields as character, pcWhere as character, pcFieldsMap as character, excludeFields as character).

      method public logical     SetDataSource (pcTable as character, pcBuffer as character, pcKeyFields as character, pcWhere as character, pcFieldsMap as character, excludeFields as character, includeFields as character).

      method public logical     SetDataSource (pcTable as character, pcBuffer as character, pcKeyFields as character, pcWhere as character, pcFieldsMap as character, excludeFields as character, includeFields as character, joinClause as character).

   end interface.
