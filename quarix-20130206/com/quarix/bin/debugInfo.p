
/*------------------------------------------------------------------------
    File        : debugInfo.p
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Andriuhan
    Created     : Wed May 30 10:54:31 EEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

run listInfo.


/* **********************  Internal Procedures  *********************** */

procedure listInfo:
   run listBuffers.
   run listDatasets.
   run listDataSources.
   run listProcedures.
   run listQueries.
   run listObjects.
end procedure.

procedure listBuffers:
   define variable hBuf as handle    no-undo.
   define variable cStr as character no-undo.

   output to value('debuginfo_buffers.txt').

   hBuf = session:first-buffer.
   do while valid-handle(hBuf):
      cStr = substitute ('Buffer: &1',hBuf:name).
      put unformatted cStr skip.
      hBuf = hBuf:next-sibling.
   end.

   output close.
end procedure.

procedure listDatasets:
   define variable hDataset as handle    no-undo.
   define variable cStr     as character no-undo.

   output to value('debuginfo_datasets.txt').

   hDataset = session:first-dataset.
   do while valid-handle(hDataset):
      cStr = substitute ('Dataset: &1 Object: &2',hDataset:serialize-name,hDataset:private-data).
      put unformatted cStr skip.
      hDataset = hDataset:next-sibling.
   end.

   output close.
end procedure.

procedure listDataSources:
   define variable hDataSource as handle    no-undo.
   define variable cStr        as character no-undo.

   output to value('debuginfo_datasources.txt').

   hDataSource = session:first-data-source.
   do while valid-handle(hDataSource):
      cStr = substitute ('DataSource: &1',hDataSource:name).
      put unformatted cStr skip.
      hDataSource = hDataSource:next-sibling.
   end.

   output close.
end procedure.

procedure listProcedures:
   define variable hProc as handle    no-undo.
   define variable cStr  as character no-undo.

   output to value('debuginfo_procedures.txt').

   hProc = session:first-procedure.
   do while valid-handle(hProc):
      cStr = substitute ('Procedure: &1',hProc:file-name).
      put unformatted cStr skip.
      hProc = hProc:next-sibling.
   end.

   output close.
end procedure.

procedure listQueries:
   define variable hQry as handle    no-undo.
   define variable cStr as character no-undo.

   output to value('debuginfo_queries.txt').

   hQry = session:first-query.
   do while valid-handle(hQry):
      cStr = substitute ('Query: &1',hQry:prepare-string).
      put unformatted cStr skip.
      hQry = hQry:next-sibling.
   end.

   output close.
end procedure.

procedure listObjects:
   define variable obj  as Progress.Lang.Object no-undo.
   define variable cStr as character            no-undo.

   output to value('debuginfo_objects.txt').

   obj = session:first-object.
   do while valid-object(obj):
      cStr = substitute ('Object: &1',obj:ToString()).
      put unformatted cStr skip.
      obj = obj:next-sibling.
   end.

   output close.
end procedure.
