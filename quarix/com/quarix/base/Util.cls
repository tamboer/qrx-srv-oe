
 /*------------------------------------------------------------------------
    File        : Util
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Tudor
    Created     : Tue Dec 09 10:57:58 EET 2008
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
&global-define url_unsafe_chars     ' <>"#%{}|~\^~~[]`':u
&global-define url_reserved_chars   '~;/?:@=&+$,':u
&global-define hex-list             '0123456789ABCDEF':u

using com.quarix.base.Util.

class com.quarix.base.Util  implements com.quarix.base.iSingleton use-widget-pool final:

	{com/quarix/data/dsindexinformation.i}

  &if keyword-all('static':u) ne ? &then
   define private static variable util as Util no-undo.

   method public static Util GetInstance():
      if not valid-object(util) then
         util = new Util().
      return util.
   end method.
   &endif

   method public logical IsEmpty (input pcValue as character):
      if pcValue = ? or
         pcValue = '':U
      then return true.

      return false.

   end method.

   method public logical IsEmpty (input pcValue as longchar):
      if pcValue = ? or
         pcValue = '':U
      then return true.

      return false.

   end method.

   method public logical IsEmpty (input piValue as integer):
      if piValue = ? or
         piValue = 0
      then return true.

      return false.

   end method.

   method public logical IsEmpty (input pdValue as date):
      if pdValue = ?
      then return true.

      return false.

   end method.

   method public logical IsEmpty (input pdValue as datetime):
      if pdValue = ?
      then return true.

      return false.

   end method.

   method public logical IsEmpty (input pdValue as datetime-tz):
      if pdValue = ?
      then return true.

      return false.

   end method.

   method public logical IsEmpty (input pdeValue as decimal):

      if pdeValue = ? or
         pdeValue = 0
      then return true.

      return false.

   end method.

   method public logical IsEmpty (input plValue as logical):

      if plValue = ?
      then return true.

      return false.

   end method.

   method public logical IsEmpty (input phValue as handle):

      if phValue = ? or
         not valid-handle(phValue)
      then return true.

      return false.

   end method.

   method public logical IsEmptyInteger (input pcValue as character):

      define variable iValue as integer no-undo.

      iValue = integer(pcValue) no-error.

      return IsEmpty(iValue).

   end method.

   method public logical IsEmptyDecimal (input pcValue as character):

      define variable dValue as decimal no-undo.

      dValue = decimal(pcValue) no-error.

      return IsEmpty(dValue).

   end method.

   method public logical IsEqual (input pdSrcValue as date, input pdTargetValue as date):

      if IsEmpty(pdSrcValue) and
         IsEmpty(pdTargetValue)
      then return true.

      return (pdSrcValue = pdTargetValue).

   end method.

   method public logical IsEqual (input pcSrcValue as character, input pcTargetValue as character):

      if IsEmpty(pcSrcValue) and
         IsEmpty(pcTargetValue)
      then return true.

      return (pcSrcValue = pcTargetValue).

   end method.

   method public logical IsEqual (input piSrcValue as integer, input piTargetValue as integer):

      if IsEmpty(piSrcValue) and
         IsEmpty(piTargetValue)
      then return true.

      return (piSrcValue = piTargetValue).

   end method.

   method public logical IsEqual (input pdeSrcValue as decimal, input pdeTargetValue as decimal):

      if IsEmpty(pdeSrcValue) and
         IsEmpty(pdeTargetValue)
      then return true.

      return (pdeSrcValue = pdeTargetValue).

   end method.

   method public character DelSpace(input pcString as character):

      define variable iNum    as integer      no-undo.
      define variable cStr    as character    no-undo.
      define variable cString	as character	no-undo.

      pcString = trim(pcString).

      if IsEmpty(pcString)
      then return pcString.

      do iNum = 1 to num-entries(pcString, ' ':U):

         cStr = entry(iNum, pcString, ' ':U).

         if cStr <> '':U and
            cStr <> ' ':U
         then cString = cString + ' ':U + cStr.
      end.

      cString = trim(cString).

      return cString.

   end method.

	method public void DisposeAndEmptyDataset(input hds as handle):

		if not valid-handle(hds)
		then return.

		hds:empty-dataset ().

		DisposeDataset(input hds).

		return.

		catch appError as Progress.Lang.Error :
			delete object appError.
			return.
		end catch.
		finally:
			delete object hds no-error.
		end finally.

	end method.

	method public void DisposeDataset(input hds as handle):

		define variable iBuff		as integer		no-undo.
		define variable iHandle		as integer		no-undo.
		define variable cTTHandles	as character	no-undo.
		define variable hTT			as handle		no-undo.
		define variable hBuf		as handle		no-undo.

		if not valid-handle(hds)
		then return.

		do iBuff = 1 to hds:num-buffers:

			hBuf = hds:get-buffer-handle(iBuff).

			if valid-handle(hBuf)
			then cTTHandles = if IsEmpty(cTTHandles) then string(hBuf) else cTTHandles + ',' + string(hBuf).
		end.

		delete object hds no-error.

		if not valid-handle(hds)
		then hds = ?.

		if not IsEmpty(cTTHandles)
    	then
    		do iHandle = 1 to num-entries(cTTHandles, ','):

    			hBuf = handle(entry(iHandle, cTTHandles, ',')) no-error.

    			if valid-handle(hBuf)
    			then
    				hTT = hBuf:table-handle no-error.

				delete object hBuf no-error.

				delete object hTT no-error.
			end.

		return.

		catch appError as Progress.Lang.Error :
			delete object appError.
			return.
		end catch.
		finally:
			delete object hds no-error.
		end finally.

	end method.

	method public character GetLogDirectory():

    	define variable cLogDirectory	as character no-undo.
    	define variable cLogFileName	as character no-undo.

		assign cLogDirectory = log-manager:logfile-name.

		if not IsEmpty(cLogDirectory)
		then
			assign
				cLogDirectory	= replace(cLogDirectory, chr(92), '~/':U)
    			cLogFileName	= entry(num-entries(cLogDirectory, '~/':U), cLogDirectory, '~/':U)
    			cLogDirectory	= substring(cLogDirectory, 1, length(cLogDirectory) - length(cLogFileName))
    			cLogDirectory	= right-trim(cLogDirectory, '~/':U)
    			no-error.
    	else
			assign
				cLogDirectory	= session:temp-directory
				cLogDirectory	= replace(cLogDirectory, chr(92), '~/':U)
				cLogDirectory	= right-trim(cLogDirectory, '~/':U)
				no-error.

		return cLogDirectory.

		catch appError as Progress.Lang.Error :
			delete object appError.
			return ?.
		end catch.

    end method.

    method public void LogDataset(input dataset-handle phDataSet):

    	LogDataset(input dataset-handle phDataSet by-reference, '':U, no, yes).

    	finally:
			delete object phDataSet no-error.
		end finally.

    end method.

    method public void LogDatasetUnformatted(input dataset-handle phDataSet):

    	LogDataset(input dataset-handle phDataSet by-reference, '':U, no, no).

    	finally:
			delete object phDataSet no-error.
		end finally.

    end method.

    method public void LogDataset(input dataset-handle phDataSet, input pcName as character):

    	LogDataset(input dataset-handle phDataSet by-reference, pcName, no, yes).

    	finally:
			delete object phDataSet no-error.
		end finally.

    end method.

    method public void LogDatasetUnformatted(input dataset-handle phDataSet, input pcName as character):

    	LogDataset(input dataset-handle phDataSet by-reference, pcName, no, no).

    	finally:
			delete object phDataSet no-error.
		end finally.

    end method.

    method public void LogDataset(input dataset-handle phDataSet, input plWriteBeforeImage as logical, input plFormatted as logical):

    	LogDataset(input dataset-handle phDataSet by-reference, '':U, plWriteBeforeImage, plFormatted).

    	finally:
			delete object phDataSet no-error.
		end finally.

    end method.

    method public void LogDataset(input dataset-handle phDataSet, input pcName as character, input plWriteBeforeImage as logical, input plFormatted as logical):

    	define variable cLogDirectory		as character	no-undo.
    	define variable cLogFileName		as character	no-undo.
    	define variable cFullLogFileName	as character	no-undo.
    	define variable iNum				as integer		no-undo.
    	define variable cFileName			as character	no-undo.

    	if not valid-handle(phDataSet)
    	then return.

    	assign
			cLogDirectory	= GetLogDirectory()
			cFileName		= phDataSet:name.

		if pcName <> '':U and
			pcName <> ?
		then cFileName = cFileName + '_':U + pcName.

		if cFileName = '' or
			cFileName = ? or
			cLogDirectory = '' or
			cLogDirectory = ?
    	then return.

    	assign
			cLogFileName		= cFileName + '.xml':U
    		cFullLogFileName	= cLogDirectory + '~/':U + cLogFileName.

    	do while search(cFullLogFileName) <> ?:

    		iNum = iNum + 1.

    		assign
				cLogFileName		= cFileName + '_':U + string(iNum) + '.xml':U
    			cFullLogFileName	= cLogDirectory + '~/':U + cLogFileName.

    	end. /* do while search(cFullLogFileName) = ? */

    	message
    		'------------------------------------------------------------------------------------------------------------------------------':U skip
    		'Write file in: ' cFullLogFileName skip
    		'------------------------------------------------------------------------------------------------------------------------------':U skip
		view-as alert-box.

    	phDataSet:write-xml('file':U, cFullLogFileName, plFormatted, 'utf-8':u, ?, no, no, plWriteBeforeImage, no).

		return.

		catch appError as Progress.Lang.Error :
         	delete object appError.
		end catch.
		finally:
			delete object phDataSet no-error.
		end finally.

    end method.

    method public void LogTempTable(input phBuffer as handle):

    	LogTempTable(input phBuffer, ?).

    	finally:
    		delete object phBuffer no-error.
    	end finally.

    end method.

    method public void LogTempTable(input phBuffer as handle, input pcName as character):

    	define variable cLogDirectory		as character	no-undo.
    	define variable cLogFileName		as character	no-undo.
    	define variable cFullLogFileName	as character	no-undo.
    	define variable iNum				as integer		no-undo.
    	define variable cFileName			as character	no-undo.

    	if not valid-handle(phBuffer)
    	then return.

    	assign
			cLogDirectory	= GetLogDirectory()
			cFileName		= phBuffer:name.

		if pcName <> '':U and
			pcName <> ?
		then cFileName = cFileName + '_':U + pcName.

		if cFileName = '' or
			cFileName = ? or
			cLogDirectory = '' or
			cLogDirectory = ?
    	then return.

    	assign
			cLogFileName		= cFileName + '.xml':U
    		cFullLogFileName	= cLogDirectory + '~/':U + cLogFileName.

    	do while search(cFullLogFileName) <> ?:

    		iNum = iNum + 1.

    		assign
				cLogFileName		= cFileName + '_':U + string(iNum) + '.xml':U
    			cFullLogFileName	= cLogDirectory + '~/':U + cLogFileName.

    	end. /* do while search(cFullLogFileName) = ? */

    	message
    		'------------------------------------------------------------------------------------------------------------------------------':U skip
    		'Write file in: ' cFullLogFileName skip
    		'------------------------------------------------------------------------------------------------------------------------------':U skip
		view-as alert-box.

    	phBuffer:write-xml('file':U, cFullLogFileName, yes).

		return.

		catch appError as Progress.Lang.Error :
         	delete object appError.
		end catch.
		finally:
			delete object phBuffer no-error.
			delete object phBuffer no-error.
		end finally.

    end method.


    method public void DumpMemPtrToFile(input mp as memptr, input pcName as character):

    	define variable cLogDirectory		as character	no-undo.
    	define variable cLogFileName		as character	no-undo.
    	define variable cFullLogFileName	as character	no-undo.
    	define variable iNum				as integer		no-undo.
    	define variable cFileName			as character	no-undo.

    	assign cLogDirectory = GetLogDirectory().

		if pcName <> '':U and
			pcName <> ?
		then cFileName = pcName.

		if cFileName = '' or
			cFileName = ? or
			cLogDirectory = '' or
			cLogDirectory = ?
    	then return.

    	assign
			cLogFileName		= cFileName + '.json':U
    		cFullLogFileName	= cLogDirectory + '~/':U + cLogFileName.

    	do while search(cFullLogFileName) <> ?:

    		iNum = iNum + 1.

    		assign
				cLogFileName		= cFileName + '_':U + string(iNum) + '.json':U
    			cFullLogFileName	= cLogDirectory + '~/':U + cLogFileName.

    	end. /* do while search(cFullLogFileName) = ? */

    	copy-lob mp to file cFullLogFileName.

    	return.

    	catch appError as Progress.Lang.Error :
         	delete object appError.
		end catch.

    end method.

   method public character Nvl
      (stText as character, stDefault as character ):

      if IsEmpty(stText) then return stDefault.

      return stText.

   end method.

	/*------------------------------------------------------------------------------
			Purpose:
			Notes:
	------------------------------------------------------------------------------*/
	method public   integer Nvl
	  ( stText as character, lDefault as integer ):
    define variable lReturn as integer    no-undo.

    if IsEmpty(stText) then return lDefault.
    assign lReturn = integer(stText) no-error.
    if error-status:error or error-status:num-messages gt 0 then return lDefault.
    return lReturn.
	end method.

	/*------------------------------------------------------------------------------
			Purpose:
			Notes:
	------------------------------------------------------------------------------*/
	method public   decimal Nvl
	  ( stText as character, lDefault as decimal ):
    define variable lReturn as decimal    no-undo.

    if IsEmpty(stText) then return lDefault.
    assign lReturn = decimal(stText) no-error.
    if error-status:error or error-status:num-messages gt 0 then return lDefault.
    return lReturn.
	end method.

	/*------------------------------------------------------------------------------
			Purpose:
			Notes:
	------------------------------------------------------------------------------*/
	method public   date Nvl
	  ( stText as character, dDefault as date ):
    define variable dReturn as date    no-undo.

    if IsEmpty(stText) then return dDefault.
    assign dReturn = date(stText) no-error.
    if error-status:error or error-status:num-messages gt 0 then return dDefault.
    return dReturn.
	end method.


	/*------------------------------------------------------------------------------
			Purpose:
			Notes:
	------------------------------------------------------------------------------*/
	method public   logical Nvl
	  ( stText as character, fDefault as logical ):
	  if isEmpty(stText) then return fDefault.
    return (lookup(stText,'y,yes,true,1':u) gt 0).
	end method.


   method public logical IsError ():
      return (error-status:error eq true or error-status:num-messages ne 0).
   end method.

   method  public character FormatDatetime
     (input stMethod   as character,
      input dtIn       as datetime-tz,
      input stDateType as character) :

     define variable stDays   as character no-undo initial 'Sun,Mon,Tue,Wed,Thu,Fri,Sat':u.
     define variable stMonths as character no-undo initial 'Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec':u.

     /* unable to format this, try to be more specific       */
     /* this causes a bug in set-cookie                      */

     if dtIn = ? then return '':u.

     if stDateType = 'local':u then
      dtIn = datetime-tz(dtIn,0).
     else
      dtIn = datetime-tz(dtIn).
     /* cookie: Sun, DD-Mon-YYYY HH:MM:SS GMT      */
     /* http:   Sun, DD Mon YYYY HH:MM:SS GMT      */
     return substitute('&1, &2&3&4&3&5 &6 GMT':u,
                       entry(weekday(dtIn), stDays),
                       string(day(dtIn),'99':u),
                       (if stMethod = 'cookie':u then '-':u else ' ':u),
                       entry(month(dtIn), stMonths),
                       string(year(dtIn), '9999':u),
                       string(integer ( mtime(dtIn) / 1000 ),'hh:mm:ss':u)).
   end method. /* format-datetime */

   method public character HtmlEncode  (stString as character):

     if IsEmpty(stString) then
        return stString.

     assign
       stString = replace(stString, '~&':u, '&amp;':u)
       stString = replace(stString, '"':u,  '&quot;':u)
       stString = replace(stString, '~'':u, '&apos;':u)
       stString = replace(stString, '~<':u, '&lt;':u)
       stString = replace(stString, '~>':u, '&gt;':u).
     return stString.
   end method.

   method public character HtmlDecode  (stString as character):

     if IsEmpty(stString) then
        return stString.

     assign
       stString = replace(stString, '&amp;':u,  '~&':u)
       stString = replace(stString, '&quot;':u, '"':u)
       stString = replace(stString, '&apos;':u, '~'':u)
       stString = replace(stString, '&lt;':u,   '~<':u)
       stString = replace(stString, '&gt;':u,   '~>':u).
     return stString.
   end method.

   method public character UrlEncode (stString as character):
      return UrlEncode(stString, '':u).
   end method.

   method public character UrlEncode (stString as character, stMethod as character):
     define variable stResult      as character no-undo.
     define variable cEncodeChars  as character no-undo.
     define variable lCount        as integer   no-undo.
     define variable cChr          as character no-undo.
     define variable lChr          as integer   no-undo.

     if IsEmpty(stString) then
        return stString.

     cEncodeChars = if stMethod = 'query':u
       then {&url_unsafe_chars} + {&url_reserved_chars}
       else if stMethod = 'cookie':u
         then {&url_unsafe_chars} + '~;,':u
         else {&url_unsafe_chars}.

     /* encode each character if it have to                                        */
     do lCount = 1 to length(stString, 'character':u):
       assign
         cChr = substring(stString, lCount, 1, 'character':u)
         lChr = ascending(cChr).
       if lChr <= 31 or lChr >= 127 or index(cEncodeChars, cChr) > 0 then do:
         /* translate the character in hexadecimal pair prepend with "%"           */
         stResult = stResult + chr(37) /* "%"  */
                     + substring({&hex-list},
                                 integer(truncate(lChr / 16, 0)) + 1,
                                 1, 'character':u)
                     + substring({&hex-list},
                                 lChr modulo 16 + 1,
                                 1, 'character':u).
       end.
       else stResult = stResult + cChr.
     end.

     return stResult.
   end method.

   method public character UrlDecode (input stString as character):
     define variable stResult      as character no-undo.
     define variable stResString   as character no-undo.
     define variable lCount        as integer   no-undo.
     define variable cChr          as character no-undo.
     define variable lChr          as integer   no-undo.

     if IsEmpty(stString) then
        return stString.

     /* decode each character if it have to                                      */
     do lCount = 1 to length(stString, 'character':u):
       cChr = substring(stString, lCount, 1, 'character':u).
       /* we found the "%" prepending the hexadecimal pair                       */
       if ascending(cChr) = 37 then do:
         /* translate the hexadecimal pair back in coresponding character          */
         assign
           lChr = 16 * (index({&hex-list}, substring(stString, lCount + 1, 1, 'character':u)) - 1)
                 + (index({&hex-list}, substring(stString, lCount + 2, 1, 'character':u)) - 1)
           stResult = stResult + chr(lChr)
           lCount = lCount + 2.
       end.
       else stResult = stResult + cChr.
     end.

     return stResult.
   end method.

   method public void Reset():
   end method.

   method public longchar Nvl
      (stText as longchar, stDefault as longchar ):

      if IsEmpty(stText) then return stDefault.
      return stText.
   end method.

   method public character FirstUpper (str as character):
      if not IsEmpty(str) then
         substring(str, 1, 1) = caps(substring(str, 1, 1)).
      return str.
   end method.

	method public integer GetNextSortOrder(input hBuf as handle):

		define variable hField		as handle		no-undo.
		define variable hQuery		as handle		no-undo.
		define variable cQuery		as character	no-undo.
		define variable lPrepareOk	as logical		no-undo.
		define variable iSortOrder	as integer		no-undo.
		define variable hBufTmp		as handle		no-undo.

		if not valid-handle(hBuf)
		then return ?.

		create buffer hBufTmp for table hBuf buffer-name hBuf:name.

		assign hField = hBufTmp:buffer-field('SortOrder':U) no-error.

		if not valid-handle(hField)
		then do:
			delete object hBufTmp no-error.

			return ?.
		end.

		create query hQuery.

		hQuery:set-buffers(hBufTmp).

		cQuery = substitute('for each &1 no-lock by &1.SortOrder descending':U, hBufTmp:name).

		lPrepareOk = hQuery:query-prepare (cQuery) no-error.

		if error-status:error or
			not lPrepareOk
		then do:
			delete object hQuery no-error.

			return ?.
		end.

		iSortOrder = 0.

		hQuery:query-open ().

		hQuery:get-first ().

		if not hQuery:query-off-end
		then iSortOrder = hField:buffer-value().

		hQuery:query-close ().

		delete object hQuery no-error.

		delete object hBufTmp no-error.

		iSortOrder = iSortOrder + 1.

		return iSortOrder.

		catch appError as Progress.Lang.Error :

			delete object hQuery	no-error.
			delete object hBufTmp	no-error.

            delete object appError.

            return ?.

        end catch.
        finally:
        	delete object hBuf no-error.
        end finally.

	end method.

	method public void DisplayErrorMessages(input apperror as Progress.Lang.Error):

		define variable iNumMsg as integer no-undo.

		if not valid-object(apperror)
		then return.

		do iNumMsg = 1 to apperror:NumMessages:
			message apperror:GetMessage(iNumMsg) skip
			view-as alert-box.
		end.

	end method.

	method public integer GetPid ():
	    define variable intProcessHandle as integer no-undo.
        define variable hProc            as handle  no-undo.

        run com/quarix/bin/pid.p persistent set hProc.

        if opsys eq 'WIN32' then
            run GetCurrentProcessId in hProc (output intProcessHandle).

        if opsys eq 'UNIX' then
            run getpid in hProc (output intProcessHandle).

        if valid-handle(hProc) then
        delete procedure hProc no-error.

        return intProcessHandle.
    end method.

    method public character ReplaceString (input cText as character, input cSource as character, input cTarget as character):
        if not IsEmpty(cText) and
           cSource ne cTarget and
           index(cText,cSource) gt 0
        then do:
           cText = trim(replace(cText,cSource,cTarget)).
        end.

        return cText.
    end method.

    method public character RemoveString (input cText as character, input cString as character):
        return ReplaceString(cText,cString,'':U).
    end method.

    method public void TrackChanges(DsDataset as handle, pcOnOff as logical):

        define variable iBuffer as integer no-undo.

        do iBuffer = 1 to Dsdataset:num-buffers:
            Dsdataset:get-buffer-handle(iBuffer):table-handle:tracking-changes = pcOnOff.
        end.

        finally:
        	delete object DsDataset no-error.
        end finally.

    end method.

    method public logical StoreRawInBuffer(input hBufTtTemp as handle, input rRaw as raw):

        define variable httDummy        as handle       no-undo.
        define variable hBufttDummy     as handle       no-undo.

        if not valid-handle(hBufTtTemp)
        then return false.

        create temp-table httDummy.

        httDummy:add-new-field ('rawvalue':U, 'raw':U).

        httDummy:temp-table-prepare ('ttdummy':U).

        hBufttDummy = httDummy:default-buffer-handle.

        hBufttDummy:empty-temp-table ().

        hBufttDummy:buffer-create ().

        hBufttDummy:buffer-field ('rawvalue':U):buffer-value = rRaw.

        do transaction
            on error undo, throw
            on stop undo, leave:

            hBufTtTemp:buffer-create ().

            hBufTtTemp:raw-transfer (false, hBufttDummy:buffer-field ('rawvalue':U)).

        end. /* do transaction */

        return true.

        catch appError as Progress.Lang.Error :
            delete object appError.
            return false.
        end catch.
        finally:
            delete object httDummy		no-error.
            delete object hBufTtTemp	no-error.
        end finally.

    end method.

    method public logical GetIndexInformation(input hBuf as handle, input dataset dsindexinformation):

        define variable hTable          as handle       no-undo.
        define variable iNumIndex       as integer      no-undo.
        define variable cIndexInfo      as character    no-undo.
        define variable cIndexName      as character    no-undo.
        define variable lUniqueIndex    as logical      no-undo.
        define variable lPrimaryIndex   as logical      no-undo.
        define variable lWordIndex      as logical      no-undo.
        define variable iNumFields      as integer      no-undo.
        define variable cFiedlName      as character    no-undo.
        define variable lDescending     as logical      no-undo.
        define variable iIndexId        as integer      no-undo.
        define variable cTableName      as character    no-undo.

        if not valid-handle(hBuf)
        then return false.

        find first ttIndexInformation
            where ttIndexInformation.BufferHandle = hBuf
            no-lock no-error.

        if available(ttIndexInformation)
        then return true.

        for each ttIndexInformation
            no-lock
            by ttIndexInformation.IndexId descending:

            iIndexId = ttIndexInformation.IndexId.

            leave.
        end.

        hTable = hBuf:table-handle no-error.

        iNumIndex = 0.

        do transaction
            on error undo, throw
            on stop undo, leave:

            #BufferIndexes:
            repeat:
                iNumIndex = iNumIndex + 1.

                cIndexInfo = hBuf:index-information(iNumIndex).

                if IsEmpty(cIndexInfo)
                then leave #BufferIndexes.

                cIndexName = entry(1, cIndexInfo, ',':U).

                assign
                    lUniqueIndex    = logical(entry(2, cIndexInfo, ',':U), '1/0':U)
                    lPrimaryIndex   = logical(entry(3, cIndexInfo, ',':U), '1/0':U)
                    lWordIndex      = logical(entry(4, cIndexInfo, ',':U), '1/0':U).

                iIndexId = iIndexId + 1.

                if valid-handle(hTable)
                then assign cTableName = hTable:table no-error.

                create ttIndexInformation.

                assign
                    ttIndexInformation.IndexId      = iIndexId
                    ttIndexInformation.TtableHandle = hTable
                    ttIndexInformation.BufferHandle = hBuf
                    ttIndexInformation.TableName    = cTableName
                    ttIndexInformation.BufferName   = hBuf:name
                    ttIndexInformation.IndexName    = cIndexName
                    ttIndexInformation.PrimaryIndex = lPrimaryIndex
                    ttIndexInformation.UniqueIndex  = lUniqueIndex
                    ttIndexInformation.WordIndex    = lWordIndex.

                do iNumFields = 5 to num-entries(cIndexInfo, ',':U):

                    assign
                        cFiedlName  = entry(iNumFields, cIndexInfo)
                        lDescending = logical(entry(iNumFields + 1, cIndexInfo), '1/0':U).

                    create ttIndexFields.

                    assign
                        ttIndexFields.IndexId           = iIndexId
                        ttIndexFields.FieldName         = cFiedlName
                        ttIndexFields.SortDescending    = lDescending.

                    iNumFields = iNumFields + 1.

                end. /* do iNumFields = 5 to num-entries(cIndexInfo, ',':U) */

            end. /* repeat */

        end. /* do transaction */

        return true.

        catch appError as Progress.Lang.Error :
            delete object appError.
            return false.
        end catch.
        finally:
        	delete object hBuf no-error.
        end finally.

    end method.

    method public character GetUniqueIndexFields(input hBuf as handle, input dataset dsindexinformation):

        define variable cRetValue   as character    no-undo.
        define variable iNumFields  as integer      no-undo.
        define variable cFieldName  as character    no-undo.

        if not valid-handle(hBuf)
        then return ?.

        if not GetIndexInformation(hBuf, input dataset dsindexinformation by-reference)
        then return ?.

        find first ttIndexInformation
            where ttIndexInformation.BufferHandle = hBuf
              and ttIndexInformation.PrimaryIndex
              and ttIndexInformation.UniqueIndex
            no-lock no-error.

        if not available(ttIndexInformation)
        then
            find first ttIndexInformation
                where ttIndexInformation.BufferHandle = hBuf
                  and ttIndexInformation.UniqueIndex
                no-lock no-error.

        if available(ttIndexInformation)
        then do:
            for each ttIndexFields
                where ttIndexFields.IndexId = ttIndexInformation.IndexId
                no-lock:

                if IsEmpty(cRetValue)
                then cRetValue = ttIndexFields.FieldName.
                else cRetValue = substitute('&1|&2':U, cRetValue, ttIndexFields.FieldName).
            end.
        end. /* if available(ttIndexInformation) */
        else do:
            do iNumFields = 1 to hBuf:num-fields:

                cFieldName = hBuf:buffer-field(iNumFields):name.

                if IsEmpty(cRetValue)
                then cRetValue = cFieldName.
                else cRetValue = substitute('&1|&2':U, cRetValue, cFieldName).

            end. /* do iNumFields = 1 to hBuf:num-fields */
        end.

        return cRetValue.

        catch appError as Progress.Lang.Error :
            delete object appError.
            return ?.
        end catch.
        finally:
        	delete object hBuf no-error.
        end finally.

    end method.

    method public handle CreateTableFromUniqueFields(input hSrcBuf as handle):

        define variable cUniqueFields   as character    no-undo.
        define variable hTtTemp         as handle       no-undo.
        define variable iNumFields      as integer      no-undo.
        define variable cFieldName      as character    no-undo.
        define variable hField          as handle       no-undo.

        if not valid-handle(hSrcBuf)                or
            not valid-handle(hSrcBuf:table-handle)  or
            hSrcBuf:table-handle:type <> 'TEMP-TABLE':U
        then return ?.

        cUniqueFields = GetUniqueIndexFields(hSrcBuf, input dataset dsindexinformation by-reference).

        if IsEmpty(cUniqueFields)
        then return ?.

        hField = hSrcBuf:buffer-field('HashCode':u) no-error.

        if valid-handle(hField) and
        	lookup('HashCode':u, cUniqueFields, '|':u) = 0
        then cUniqueFields = cUniqueFields + '|':u + 'HashCode':u.

        create temp-table hTtTemp.

        do iNumFields = 1 to num-entries(cUniqueFields, '|':U):

            assign
                cFieldName  = entry(iNumFields, cUniqueFields, '|':U)
                hField      = hSrcBuf:buffer-field (cFieldName).

            hTtTemp:add-like-field (cFieldName, hField).

        end. /* do iNumFields = 1 to num-entries(cUniqueFields, '|':U) */

        hTtTemp:temp-table-prepare (hSrcBuf:table-handle:name).

        return hTtTemp.

        catch appError as Progress.Lang.Error :

            delete object hTtTemp	no-error.

            delete object appError.
            return ?.
        end catch.
        finally:
        	delete object hSrcBuf no-error.
        end finally.

    end method.

    method public character GetBufferRawValue(input hBuf as handle):

        define variable httDummy        as handle       no-undo.
        define variable hBufttDummy     as handle       no-undo.
        define variable cStringValue    as character    no-undo.
        define variable rRawValue       as raw          no-undo.

        if not valid-handle(hBuf) or
            not hBuf:available
        then return ?.

        create temp-table httDummy.

        httDummy:add-new-field ('rawvalue':U, 'raw':U).

        httDummy:temp-table-prepare ('ttdummy':U).

        hBufttDummy = httDummy:default-buffer-handle.

        hBufttDummy:empty-temp-table ().

        hBufttDummy:buffer-create ().

        hBuf:raw-transfer (true, hBufttDummy:buffer-field ('rawvalue':U)).

        rRawValue = hBufttDummy:buffer-field ('rawvalue':U):buffer-value ().

        cStringValue = hex-encode (rRawValue).

        return cStringValue.

        catch appError as Progress.Lang.Error :
            delete object appError.
            return ?.
        end catch.
        finally:
            delete object httDummy	no-error.
            delete object hBuf		no-error.
        end finally.

    end method.

    method public character GetSourceBufferDbRowIDTempTable(input hSrcBuf as handle):

    	define variable cRowId          as character    no-undo.
    	define variable cUniqueFields   as character    no-undo.
        define variable iNumFields      as integer      no-undo.
        define variable cFieldName      as character    no-undo.
        define variable hField          as handle       no-undo.
        define variable hTtTemp         as handle       no-undo.
        define variable hBufTtTemp      as handle       no-undo.
        define variable hFieldSrc       as handle       no-undo.

    	if not valid-handle(hSrcBuf)
        then return ?.

        if hSrcBuf:table-handle:type <> 'TEMP-TABLE':U
        then return ?.

        if not hSrcBuf:available
		then return '?':U.

		cUniqueFields = GetUniqueIndexFields(hSrcBuf, input dataset dsindexinformation by-reference).

		if IsEmpty(cUniqueFields)
		then return ?.

		hFieldSrc = hSrcBuf:buffer-field('HashCode':u) no-error.

		if valid-handle(hFieldSrc) and
			lookup('HashCode':u, cUniqueFields, '|':u) = 0
		then cUniqueFields = cUniqueFields + '|':u + 'HashCode':u.

		hTtTemp = CreateTableFromUniqueFields(input hSrcBuf).

		if not valid-handle(hTtTemp)
		then return ?.

		hBufTtTemp = hTtTemp:default-buffer-handle.

		hBufTtTemp:empty-temp-table ().

		hBufTtTemp:buffer-create().

		do iNumFields = 1 to num-entries(cUniqueFields, '|':U):

			assign
            	cFieldName          = entry(iNumFields, cUniqueFields, '|':U)
                hFieldSrc           = hSrcBuf:buffer-field (cFieldName)
                hField              = hBufTtTemp:buffer-field (cFieldName)
                hField:buffer-value = hFieldSrc:buffer-value.

		end. /* do iNumFields = 1 to num-entries(cUniqueFields, '|':U) */

		cRowId = GetBufferRawValue(hBufTtTemp).

		if IsEmpty(cRowId)
		then return ?.

		return cRowId.

		catch appError as Progress.Lang.Error:
            delete object appError.
            return ?.
        end catch.
        finally:
            delete object hTtTemp	no-error.
            delete object hSrcBuf	no-error.
        end finally.

    end method.

end class.
