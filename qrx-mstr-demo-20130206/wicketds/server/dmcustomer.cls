
routine-level on error undo, throw.

using Progress.Lang.*.
using com.quarix.data.DataModel.

&GLOBAL-DEFINE DefaultDatasetHandle dataset dscustomer:handle

class wicketds.server.dmcustomer inherits DataModel:

	{wicketds/server/dscustomer.i}

	{wicketds/server/dssalesrep.i}

	{wicketds/server/dsstate.i}

	def private property dmsalesrep as wicketds.server.dmsalesrep no-undo
    get:
        if not valid-object(dmsalesrep) then dmsalesrep = cast (GetInstance ('wicketds.server.dmsalesrep':u), wicketds.server.dmsalesrep).
        return dmsalesrep.
    end.
    private set.

    def private property dmstate as wicketds.server.dmstate no-undo
    get:
        if not valid-object(dmstate) then dmstate = cast (GetInstance ('wicketds.server.dmstate':u), wicketds.server.dmstate).
        return dmstate.
    end.
    private set.

	constructor public dmcustomer (  ):

    	DataAccess      = cast(GetInstance('wicketds.server.dacustomer':u), 'wicketds.server.dacustomer':u).
        datasetHandle   = {&DefaultDatasetHandle}.
        batchSize       = 0.
        id              = 'daocustomer'.
        OpenOnInit      = false.
        SendChangesOnly = false.

        if not valid-object(DataAccess) or
        	not valid-handle(datasetHandle)
        then return error.

	end constructor.

	method public override logical dataFetch ():

		DataContext:setAllRecords(true).

		return super:dataFetch().

	end method.

	method public logical getbycustomeridlist(input customeridlist as wicketds.server.customeridlist,
                                                output dataset dscustomer):

		return dataFetchIdList(input customeridlist, output dataset dscustomer by-reference).

    end method.

    method private logical AfterLoadData (input-output dataset dscustomer):

    	define variable salesrepidlist	as wicketds.server.salesrepidlist	no-undo.
    	define variable stateidlist		as wicketds.server.stateidlist		no-undo.

    	salesrepidlist = cast(GetInstance('wicketds.server.salesrepidlist':u), 'wicketds.server.salesrepidlist':u).

    	stateidlist = cast(GetInstance('wicketds.server.stateidlist':u), 'wicketds.server.stateidlist':u).

    	for each ttcustomer
    		no-lock:

    		salesrepidlist:addSalesRep(ttcustomer.SalesRep).

    		stateidlist:addStateCode(ttcustomer.State).
    	end.

    	if salesrepidlist:IdListHasData()
		then do:
			dmsalesrep:ClearContext().

			if not dmsalesrep:getbysalesrepidlist(salesrepidlist, output dataset dssalesrep by-reference)
			then return false.

			for each ttcustomer
				no-lock:

				find first ttsalesrep
					where ttsalesrep.SalesRep = ttcustomer.SalesRep
					no-lock no-error.

				if available(ttsalesrep)
				then ttcustomer.SalesRepName = ttsalesrep.RepName.
			end.

		end. /* if salesrepidlist:IdListHasData() */

		if stateidlist:IdListHasData()
		then do:
			if not dmstate:getbystateidlist(stateidlist, output dataset dsstate by-reference)
			then return false.

			for each ttcustomer
				no-lock:

				find first ttstate
					where ttstate.StateCode = ttcustomer.State
					no-lock no-error.

				if available(ttstate)
				then ttcustomer.StateName = ttstate.StateName.
			end.

		end. /* if stateidlist:IdListHasData() */

		return true.

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return false.
		end catch.
		finally:
			UnloadInstance(stateidlist).
			UnloadInstance(salesrepidlist).
		end finally.

	end method.

    method public override logical AfterLoadData():

		return AfterLoadData(input-output dataset-handle datasetHandle by-reference).

	end method.

end class.
