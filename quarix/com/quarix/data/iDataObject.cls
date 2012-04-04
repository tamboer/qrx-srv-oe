
/*------------------------------------------------------------------------
    File        : iDataObject
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Andriuhan
    Created     : Wed Feb 29 09:01:38 EET 2012
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
using com.quarix.data.*.
using com.quarix.web.*.

interface com.quarix.data.iDataObject:

    method public logical BeforeRequest (actionName as character).

    method public logical AfterRequest  (actionName as character).

    method public logical BeforeDataOutput ().

    method public logical CheckMandatory(hBuf as handle, pcField as character).

    method public void ClearContext().

    method public void ClearFilters().

    method public void ClearProperties().

    method public void ClearProperty(PropertyName as character).

    method public void ClearQueryData().

    method public void ClearSort().

    method public logical dataFetch().

    method public logical dataFetch(output dataset-handle dsHandle).

    method public logical dataFetchIdList(idlist as IdList, output dataset-handle hDsDataset).

    method public logical dataFetchIdList(idlist as IdList, output dataset-handle hDsDataset, input QueryName as character).

    method public logical dataUpdate().

    method public logical dataUpdate(input-output dataset-handle dsHandle).

    method public void DisableLogResponse().

    method public void EnableLogResponse ().

    method public void DumpToXml().

    method public void DumpToXml(pcName as character).

    method public handle getDefaultDatasetHandle().

    method public memptr    GetDescription(responseFormat as character, applicationPath as character).

    method public character GetFilterValue(tableName as character, fieldname as character, operName as character).

    method public handle    GetIdListTmpHandle().

    method public character GetProperty(PropertyName as character).

    method public character GetQueryName().

    method public logical HandleRequest(actionName as character, webRequest as Request, webResponse as Response).

    method public logical IsFirstBatch().

    method public logical IsLastBatch().

    method public logical LoadRequestData().

    method public logical PropertyAvailable().

    method public logical PropertyAvailable(PropertyName as character).

    method public void RemoveFilter (tableName as character, fieldName as character).

    method public void RemoveSort   (tableName as character, fieldName as character).

    method public void RemoveTableFilters (tableName as character).

    method public void RemoveTableSort    (tableName as character).

    method public void restoreDatasetHandle().

    method public logical SaveData().

    method public logical SetBatchSize().

    method public logical SetBatchSizeFromRequest().

    method public logical SetFilter (tableName as character, fieldName as character, operName as character, fieldValue as character).

    method public logical SetFiltersFromRequest().

    method public logical SetRequestFilters(tableName as character, considerSearchFilter as logical).

    method public logical SetSort(tableName as character, fieldName as character, descendingSort as logical).

    method public logical SetSortFromRequest().

    method public void SetDataContext (dtContext as DataContext).

    method public void SetIdListTmpHandle(PropertyValue as handle).

    method public void SetParentBatchSize(parentBatchSize as integer).

    method public void SetProperty(PropertyName as character, PropertyValue as character).

    method public void SetQueryName(PropertyValue as character).

    method public void SetSessionParameters().

end interface.