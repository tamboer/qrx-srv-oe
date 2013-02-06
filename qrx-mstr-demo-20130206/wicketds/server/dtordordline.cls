
routine-level on error undo, throw.

using Progress.Lang.*.

&GLOBAL-DEFINE DefaultDatasetHandle dataset dsordordline:handle

class wicketds.server.dtordordline inherits com.quarix.data.DataTransaction:

	{wicketds/server/dsordordline.i}

	def private property dmorder as wicketds.server.dmorder no-undo
    get:
        if not valid-object(dmorder) then dmorder = cast (GetInstance ('wicketds.server.dmorder':u), wicketds.server.dmorder).
        return dmorder.
    end.
    private set.

    def private property dmorderline as wicketds.server.dmorderline no-undo
    get:
        if not valid-object(dmorderline) then dmorderline = cast (GetInstance ('wicketds.server.dmorderline':u), wicketds.server.dmorderline).
        return dmorderline.
    end.
    private set.

    def private property dsorderlinewrapper as wicketds.server.dsorderlinewrapper no-undo
    get:
        if not valid-object(dsorderlinewrapper) then dsorderlinewrapper = cast (GetInstance ('wicketds.server.dsorderlinewrapper':u), wicketds.server.dsorderlinewrapper).
        return dsorderlinewrapper.
    end.
    private set.

    constructor public dtordordline (  ):

        datasetHandle   = {&DefaultDatasetHandle}.
        id             = 'daordordline':U.
        batchSize       = 0.
        OpenOnInit      = false.

        if not valid-handle(datasetHandle)
        then return error.

	end constructor.

	method protected handle getdsOrderLineTmpHandle():

		define variable hDs as handle no-undo.

		assign hDs = dsorderlinewrapper:getDSHandle().

		if valid-handle(hDs)
		then do:
			Util:TrackChanges(hDs, no).

			hDs:empty-dataset().
		end.

		return hDs.

	end method.

	method private logical SaveData(input-output dataset dsordordline):

		define variable orderidlist		as wicketds.server.orderidlist	no-undo.
		define variable hdsOrderLine	as handle						no-undo.
		define variable hdsOrderTmp		as handle						no-undo.
		define variable hdsOrderLineTmp	as handle						no-undo.
		define variable httOrderLine	as handle						no-undo.
		define variable hQuery			as handle						no-undo.
		define variable iSortOrder		as integer						no-undo.
		define variable iLinenum		as integer						no-undo.

		orderidlist = cast(GetInstance('wicketds.server.orderidlist':u), 'wicketds.server.orderidlist':u).

		hdsOrderLine = getdsOrderLineTmpHandle().

		if not valid-handle(hdsOrderLine)
    	then return false.

    	httOrderLine = hdsOrderLine:get-buffer-handle (1).

    	if not valid-handle(httOrderLine)
    	then return false.

		for each btorder
			where row-state(btorder) = row-deleted
			no-lock:

			orderidlist:addOrderNum(btorder.Ordernum).
		end.

		if orderidlist:IdListHasData()
		then do:
    		dmorderline:ClearContext().

    		if not dmorderline:getbyorderidlist(orderidlist, output dataset-handle hdsOrderLine by-reference)
    		then return false.

    		create query hQuery.

    		hQuery:set-buffers (httOrderLine).

    		hQuery:query-prepare (substitute('for each &1 no-lock', httOrderLine:name)).

    		hQuery:query-open ().

    		hQuery:get-first ().

    		for each ttorderline
    			no-lock
    			by ttorderline.SortOrder descending:

    			iSortOrder = ttorderline.SortOrder.

    			leave.
    		end.

    		do while not hQuery:query-off-end:

    			find first btorderline
    				where btorderline.Ordernum	= httOrderLine:buffer-field ('Ordernum'):buffer-value ()
    				  and btorderline.Linenum	= httOrderLine:buffer-field ('Linenum'):buffer-value ()
    				no-lock no-error.

    			if not available(btorderline)
    			then do:
    				temp-table ttorderline:tracking-changes = no.

    				find first ttorderline
    					where ttorderline.Ordernum	= httOrderLine:buffer-field ('Ordernum'):buffer-value ()
    				  	  and ttorderline.Linenum	= httOrderLine:buffer-field ('Linenum'):buffer-value ()
    					no-lock no-error.

    				if available(ttorderline)
    				then delete ttorderline.

    				iSortOrder = iSortOrder + 1.

    				create ttorderline.

    				buffer ttorderline:buffer-copy (httOrderLine, 'SortOrder').

    				ttorderline.SortOrder = iSortOrder.

    				temp-table ttorderline:tracking-changes = yes.

    				delete ttorderline.

    				temp-table ttorderline:tracking-changes = no.

    			end. /* if not available(btorderline) */

    			hQuery:get-next ().

    		end. /* do while not hQuery:query-off-end: */

    		hQuery:query-close ().

    		delete object hQuery no-error.

    		hdsOrderLine:empty-dataset ().

		end. /* if orderidlist:IdListHasData() */

		find first ttorderline
			where row-state(ttorderline) = row-created
			no-lock no-error.

		if available(ttorderline)
		then do:
			dmorderline:ClearContext().

			dmorderline:SetSort('ttOrderline', 'Linenum', yes).

			dmorderline:DataContext:setBatchSize('ttOrderline', 1).

			if not dmorderline:dataFetch(output dataset-handle hdsOrderLine by-reference)
			then return false.

			httOrderLine:find-first ('', no-lock) no-error.

			if httOrderLine:available
			then iLinenum = httOrderLine:buffer-field ('Linenum'):buffer-value ().

			hdsOrderLine:empty-dataset ().

			temp-table ttorderline:tracking-changes = no.

			for each ttorderline
				where row-state(ttorderline) = row-created
				no-lock:

				iLinenum = iLinenum + 1.

				ttorderline.Linenum = iLinenum.
			end.

		end. /* if available(ttorderline) */

		wicketds.server.dsorderwrapper:createOrder(
    		input dataset dsordordline by-reference,
    		this-object:ToString(),
    		output dataset-handle hdsOrderTmp by-reference
    		).

		wicketds.server.dsorderwrapper:createOrderLine(
    		input dataset dsordordline by-reference,
    		this-object:ToString(),
    		output dataset-handle hdsOrderLineTmp by-reference
    		).

    	if not valid-handle(hdsOrderLineTmp) or
    		not valid-handle(hdsOrderTmp)
		then return false.

		do transaction
			on error undo, throw:

			dmorder:ClearContext().

			dmorderline:ClearContext().

			if not dmorder:dataUpdate(input-output dataset-handle hdsOrderTmp by-reference)
			then undo, return false.

			if not dmorderline:dataUpdate(input-output dataset-handle hdsOrderLineTmp by-reference)
			then undo, return false.
		end.

		finally:
			UnloadInstance(orderidlist).

			Util:DisposeDataset(hdsOrderTmp).
			Util:DisposeDataset(hdsOrderLineTmp).
			Util:DisposeDataset(hdsOrderLine).

			delete object hQuery no-error.

		end finally.

	end method.

	method public override logical SaveData():

        return SaveData(input-output dataset-handle datasetHandle by-reference).

    end method.

end class.
