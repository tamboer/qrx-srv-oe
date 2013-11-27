
 /*------------------------------------------------------------------------
    File        : dswrapper
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Adam
    Created     : Thu Jun 30 14:58:01 EEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

routine-level on error undo, throw.

using Progress.Lang.*.

class com.quarix.data.dswrapper inherits com.quarix.base.BaseObject implements com.quarix.base.iDisposable:

	{com/quarix/data/ttbufhandle.i}

	define protected property dsHandle as handle no-undo
		get.
		set.

	constructor	dswrapper():

		super().

	end constructor.

	destructor public dswrapper ( ):

		define variable iNumBuf	as integer	no-undo.
		define variable hBuf	as handle	no-undo.
		define variable htt		as handle	no-undo.

		if valid-handle(dsHandle)
		then do:
			Util:TrackChanges(dsHandle, false).

			dsHandle:empty-dataset ().

			do iNumBuf = 1 to dsHandle:num-buffers:

				hBuf = dsHandle:get-buffer-handle (iNumBuf).

				htt = hBuf:table-handle.

				delete object hBuf no-error.

				delete object htt no-error.
			end.

			delete object dsHandle no-error.

			if valid-handle(dsHandle)
			then dsHandle = ?.

		end. /* if valid-handle(datasetHandle) */

	end destructor.

	method public handle getDsHandle():
		return dsHandle.
	end method.

	method public handle createDynamic(input table ttbufhandle):

		define variable hDataSet	as handle	no-undo.
		define variable hBufHandle	as handle	no-undo.

		create dataset hDataSet.

		if not valid-handle(hDataSet)
		then return ?.

		assign
			hDataSet:name			= 'dsWrapper':u
			hDataSet:private-data	= this-object:ToString().

		for each ttbufhandle
			no-lock:

			hBufHandle = ttbufhandle.hBufHandle.

			if valid-handle(hBufHandle)
			then hDataSet:add-buffer(hBufHandle).
		end.

		return hDataSet.

		catch appError as Progress.Lang.Error :

			delete object hDataSet no-error.

			ThrowError(input appError).
			delete object appError.
			return ?.
		end catch.

	end method.

	method public static handle createDynamic(input cBufHandleList as character):

		return createDynamic(input cBufHandleList, input '':u).

		catch appError as Progress.Lang.Error :
			delete object appError.
			return ?.
		end catch.

	end method.

	method public static handle createDynamic(input cBufHandleList as character, input cSourceObject as character):

		define variable hDataSet	as handle	no-undo.
		define variable hBufHandle	as handle	no-undo.
		define variable iNum		as integer	no-undo.

		create dataset hDataSet.

		if not valid-handle(hDataSet)
		then return ?.

		assign
			hDataSet:name			= 'dsWrapper':u
			hDataSet:private-data	= cSourceObject.

		do iNum = 1 to num-entries(cBufHandleList, '|':U):

			hBufHandle = handle(entry(iNum, cBufHandleList, '|':U)) no-error.

			if valid-handle(hBufHandle)
			then hDataSet:add-buffer(hBufHandle).
		end.

		return hDataSet.

		catch appError as Progress.Lang.Error :

			delete object hDataSet no-error.

			delete object appError.
			return ?.
		end catch.

	end method.

	method public static logical createDataset(input dataset-handle hdsSource, input cSrcBufList as character, output dataset-handle hdsResult):

		return createDataset(input dataset-handle hdsSource by-reference, input cSrcBufList, input '':u, output dataset-handle hdsResult by-reference).

		catch appError as Progress.Lang.Error :

			delete object hdsResult no-error.

			delete object appError.

			return false.

		end catch.
		finally:
			delete object hdsSource no-error.
			delete object hdsResult no-error.
		end finally.

	end method.

	method public static logical createDataset(input dataset-handle hdsSource, input cSrcBufList as character, input cSourceObject as character, output dataset-handle hdsResult):

		define variable hSrcBufHandle	as handle		no-undo.
		define variable hBufHandle		as handle		no-undo.
		define variable iNum			as integer		no-undo.
		define variable cBufName		as character	no-undo.
		define variable cBufHandleList	as character	no-undo.

		do iNum = 1 to num-entries(cSrcBufList, '|':U):

			cBufName = entry(iNum, cSrcBufList, '|':U).

			cBufName = trim(cBufName).

			if cBufName = '' or
				cBufName = ?
			then next.

			hSrcBufHandle = hdsSource:get-buffer-handle(cBufName) no-error.

			if valid-handle(hSrcBufHandle)
			then do:
				create buffer hBufHandle for table hSrcBufHandle buffer-name hSrcBufHandle:name no-error.

				if valid-handle(hBufHandle)
				then do:
					hBufHandle:private-data = cSourceObject.

					cBufHandleList =	if (cBufHandleList = '':U or cBufHandleList = ?)
										then string(hBufHandle)
										else cBufHandleList + '|':U + string(hBufHandle).
				end.
			end.

		end. /* do iNum = 1 to num-entries(cSrcBufList, '|':U) */

		hdsResult = createDynamic(cBufHandleList, cSourceObject).

		if not valid-handle(hdsResult)
		then do:
			DeleteObjects(cBufHandleList).

			return false.
		end.

		return true.

		catch appError as Progress.Lang.Error :

			DeleteObjects(cBufHandleList).

			delete object hBufHandle no-error.

			delete object hdsResult no-error.

			delete object appError.
			return false.
		end catch.
		finally:
			delete object hdsSource no-error.
			delete object hdsResult no-error.
		end finally.

	end method.

	method private static void DeleteObjects(input cBufHandleList as character):

		define variable iNum			as integer		no-undo.
		define variable hBufHandle		as handle		no-undo.

		if cBufHandleList = '' or
			cBufHandleList = ?
		then return.

		do iNum = 1 to num-entries(cBufHandleList, '|':U):

			assign hBufHandle = handle(entry(iNum, cBufHandleList, '|':U)) no-error.

			if valid-handle(hBufHandle)
			then delete object hBufHandle no-error.

		end. /* do iNum = 1 to num-entries(cBufHandleList, '|':U) */

	end method.

end class.
