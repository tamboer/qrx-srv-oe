
define variable dmcustomer      as wicketds.server.dmcustomer   no-undo.
define variable mainController  as com.quarix.system.Controller no-undo.
define variable cGuid			as character					no-undo.
define variable cAppName		as character					no-undo.

{wicketds/server/dscustomer.i}

dmcustomer = new wicketds.server.dmcustomer().

dmcustomer:ClearContext().

dmcustomer:ErrorManager:Purge().

run com/quarix/bin/getcontroller.p(output mainController).

assign
    cGuid		= guid
	cAppName	= 'qrxexmpl':u.

mainController:Application:Name = cAppName.
mainController:Application:StartSession(cGuid, 1800).

dmcustomer:SetFilter('ttcustomer':U, 'Name':U, '=':U, 'Lift Tours').

if not dmcustomer:DataFetch(output dataset dscustomer by-reference)
then do:
    dataset dscustomer:empty-dataset ().

    run GetQuarixErrors.

	return.
end.

procedure GetQuarixErrors:

    define variable hErrBuf			as handle		no-undo.
    define variable hQuery			as handle		no-undo.
    define variable cErrorCode		as character	no-undo.
    define variable cErrorMessage	as character	no-undo.
    define variable hTmpError		as handle		no-undo.
    define variable iErrorLevel		as integer		no-undo.

    hTmpError = dmcustomer:ErrorManager:GetTmpErrorHandle().

    if not valid-handle(hTmpError)
    then return.

    hErrBuf = hTmpError:default-buffer-handle.

    create query hQuery.

    	hQuery:set-buffers (hErrBuf).

    	hQuery:query-prepare (substitute('for each &1 no-lock where &1.errorLevel = 0 or &1.errorLevel = 1':U, hErrBuf:name)).

    	hQuery:query-open ().

    	hQuery:get-first ().

    	do while not hQuery:query-off-end:

    		assign
    			cErrorCode		= string(hErrBuf:buffer-field ('errorCode':U):buffer-value ())
    			cErrorMessage	= hErrBuf:buffer-field ('errorMessage':U):buffer-value ()
    			iErrorLevel		= hErrBuf:buffer-field ('errorLevel':U):buffer-value ().

    		hQuery:get-next ().

    	end. /* do while not hQuery:query-off-end */

    	hQuery:query-close ().

    	finally:
    		delete object hQuery no-error.
    	end finally.

end procedure.

finally:
    delete object dmcustomer no-error.
end finally.
