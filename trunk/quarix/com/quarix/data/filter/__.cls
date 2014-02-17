
/*------------------------------------------------------------------------
   File        : __
   Purpose     :
   Syntax      :
   Description :
   Author(s)   : Adam
   Created     : Thu Feb 06 15:21:14 EET 2014
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

routine-level on error undo, throw.

using Progress.Lang.*.
using com.quarix.data.filter.__.
using com.quarix.data.filter.operator.
using com.quarix.data.filter.isop.

class com.quarix.data.filter.__:

	define private static temp-table ttFilterObject no-undo
		field oFilterEntry as Progress.Lang.Object.

	define private variable FieldName       	as character                    no-undo.
	define private variable Operator        	as character                    no-undo.
	define private variable FilterValue			as character                    no-undo.
	define private variable FilterObject		as com.quarix.data.filter.__	no-undo.
	define private variable FilterRelation		as character					no-undo.
	define private variable PrevFilterObject	as com.quarix.data.filter.__	no-undo.
	define private variable NextFilterObject    as com.quarix.data.filter.__	no-undo.

	constructor private __():
	end constructor.

	method private logical addCondition(input pcFieldName as character, input pcOperator as character, input pcFilterValue as character, input pcFilterRelation as character, input poPrevFilterObject as com.quarix.data.filter.__):

		if pcFieldName = ?		or
			pcFieldName = ''	or
			pcOperator = ?		or
			pcOperator = ''		or
			pcFilterValue = ?	or
			pcFilterValue = ''
		then return false.

		if pcFilterRelation <> '' and
			pcFilterRelation <> ?
		then
			if pcFilterRelation <> isop:AND_:ToString() and
				pcFilterRelation <> isop:OR_:ToString()
			then return false.

		assign
			FieldName		= pcFieldName
			Operator		= pcOperator
			FilterValue		= pcFilterValue
			FilterRelation	= pcFilterRelation.

		if valid-object(poPrevFilterObject)
		then PrevFilterObject = poPrevFilterObject.

		return true.

		catch appError as Progress.Lang.Error :
			delete object appError.
			return false.
		end catch.

	end method.

	method private logical addCondition(input oFilterObject as com.quarix.data.filter.__, input pcFilterRelation as character, input poPrevFilterObject as com.quarix.data.filter.__):

		define variable oPrevFilterObject as com.quarix.data.filter.__ no-undo.

		if not valid-object(oFilterObject)
		then return false.

		if pcFilterRelation <> '' and
			pcFilterRelation <> ?
		then
			if pcFilterRelation <> isop:AND_:ToString() and
				pcFilterRelation <> isop:OR_:ToString()
			then return false.

		do while valid-object(oFilterObject):

			oPrevFilterObject = oFilterObject:GetFilterEntryParent().

			if valid-object(oPrevFilterObject)
			then oFilterObject = oPrevFilterObject.
			else leave.
		end.

		assign
			FilterObject	= oFilterObject
			FilterRelation	= pcFilterRelation.

		if valid-object(poPrevFilterObject)
		then PrevFilterObject = poPrevFilterObject.

		return true.

		catch appError as Progress.Lang.Error :
			delete object appError.
			delete object NextFilterObject no-error.
			return false.
		end catch.

	end method.

	method public __ GetFilterEntryParent():
		return PrevFilterObject.
	end method.

	method public logical _addCondition(input pcFieldName as character, input oIs as com.quarix.data.filter.isop, input pcFilterValue as character, input oIsRel as com.quarix.data.filter.isop, input poPrevFilterObject as com.quarix.data.filter.__):

		define variable cFilterRelation as character no-undo.

		if not valid-object(oIs)
		then return false.

		if valid-object(oIsRel)
		then cFilterRelation = oIsRel:ToString().

		return addCondition(
			input pcFieldName,
			input oIs:ToString(),
			input pcFilterValue,
			input cFilterRelation,
			input poPrevFilterObject
			).

		catch appError as Progress.Lang.Error :
			delete object appError.
			return false.
		end catch.

	end method.

	method public logical _addCondition(input oFilterObject as com.quarix.data.filter.__, input oIsRel as com.quarix.data.filter.isop, input poPrevFilterObject as com.quarix.data.filter.__):

		define variable cFilterRelation as character no-undo.

		if valid-object(oIsRel)
		then cFilterRelation = oIsRel:ToString().

		return addCondition(
			input oFilterObject,
			input cFilterRelation,
			input poPrevFilterObject
			).

		catch appError as Progress.Lang.Error :
			delete object appError.
			return false.
		end catch.

	end method.

	method private __ addNextCondition(input pcFieldName as character, input oIs as com.quarix.data.filter.isop, input pcFilterValue as character, input oIsRel as com.quarix.data.filter.isop):

		define variable oNextFilterObject as com.quarix.data.filter.__ no-undo.

		oNextFilterObject = new com.quarix.data.filter.__().

		NextFilterObject = oNextFilterObject.

		if not NextFilterObject:_addCondition(
			input pcFieldName,
			input oIs,
			input pcFilterValue,
			input oIsRel,
			input this-object
			)
		then do:
			delete object NextFilterObject no-error.
			return ?.
		end.

		return NextFilterObject.

		catch appError as Progress.Lang.Error :
			delete object appError.
			delete object NextFilterObject no-error.
			return ?.
		end catch.

	end method.

	method private __ addNextCondition(input oFilterObject as com.quarix.data.filter.__, input oIsRel as com.quarix.data.filter.isop):

		define variable oNextFilterObject as com.quarix.data.filter.__ no-undo.

		oNextFilterObject = new com.quarix.data.filter.__().

		NextFilterObject = oNextFilterObject.

		if not NextFilterObject:_addCondition(
			input oFilterObject,
			input oIsRel,
			input this-object
			)
		then do:
			delete object NextFilterObject no-error.
			return ?.
		end.

		return NextFilterObject.

		catch appError as Progress.Lang.Error :
			delete object appError.
			delete object NextFilterObject no-error.
			return ?.
		end catch.

	end method.

	method public __ And_(input pcFieldName as character, input oIs as com.quarix.data.filter.isop, input pcFilterValue as character):
		return addNextCondition(
			input pcFieldName,
			input oIs,
			input pcFilterValue,
			input com.quarix.data.filter.isop:AND_
			).
	end method.

	method public __ And_(input ottfield as com.quarix.data.ttfield, input oIs as com.quarix.data.filter.isop, input pcFilterValue as character):
		return And_(input ottfield:getFqName(), input oIs, input pcFilterValue).
	end method.

	method public __ And_(input oFilterObject as com.quarix.data.filter.__):
		return addNextCondition(
			input oFilterObject,
			input com.quarix.data.filter.isop:AND_
			).
	end method.

	method public __ Or_(input pcFieldName as character, input oIs as com.quarix.data.filter.isop, input pcFilterValue as character):
		return addNextCondition(
			input pcFieldName,
			input oIs,
			input pcFilterValue,
			input com.quarix.data.filter.isop:OR_
			).
	end method.

	method public __ Or_(input ottfield as com.quarix.data.ttfield, input oIs as com.quarix.data.filter.isop, input pcFilterValue as character):
		return Or_(input ottfield:getFqName(), input oIs, input pcFilterValue).
	end method.

	method public __ Or_(input oFilterObject as com.quarix.data.filter.__):
		return addNextCondition(
			input oFilterObject,
			input com.quarix.data.filter.isop:OR_
			).
	end method.

	method static public __ In_(input oFilterObject as com.quarix.data.filter.__):

		define variable oFilterEntry as com.quarix.data.filter.__ no-undo.

		if not valid-object(oFilterObject)
		then return ?.

		oFilterEntry = new com.quarix.data.filter.__().

		if not oFilterEntry:_addCondition(
			input oFilterObject,
			input ?,
			input ?)
		then do:
			delete object oFilterEntry no-error.
			return ?.
		end.

		create ttFilterObject.

		ttFilterObject.oFilterEntry = oFilterEntry.

        return oFilterEntry.

		catch appError as Progress.Lang.Error :
			delete object appError.
			delete object oFilterEntry no-error.
			return ?.
		end catch.

	end method.

	method static public __ On_(input pcFieldName as character, input oIs as com.quarix.data.filter.isop, input pcFilterValue as character):

		define variable oFilterEntry as com.quarix.data.filter.__ no-undo.

		oFilterEntry = new com.quarix.data.filter.__().

		if not oFilterEntry:_addCondition(
			input pcFieldName,
			input oIs,
			input pcFilterValue,
			input ?,
			input ?
			)
		then do:
			delete object oFilterEntry no-error.
			return ?.
		end.

		create ttFilterObject.

		ttFilterObject.oFilterEntry = oFilterEntry.

		return oFilterEntry.

		catch appError as Progress.Lang.Error :
			delete object appError.
			delete object oFilterEntry no-error.
			return ?.
		end catch.

	end method.

	method static public __ On_(input ottfield as com.quarix.data.ttfield, input oIs as com.quarix.data.filter.isop, input pcFilterValue as character):
		return On_(
			input ottfield:getFqName(),
			input oIs,
			input pcFilterValue
			).
	end method.

	method public character DisplayQuery():

		define variable oFilterObject		as com.quarix.data.filter.__ no-undo.
		define variable oPrevFilterObject	as com.quarix.data.filter.__ no-undo.

		oFilterObject = this-object.

		do while valid-object(oFilterObject):

			oPrevFilterObject = oFilterObject:GetFilterEntryParent().

			if valid-object(oPrevFilterObject)
			then oFilterObject = oPrevFilterObject.
			else leave.
		end.

		return oFilterObject:DisplayFilterEntry().

		catch appError as Progress.Lang.Error :
			delete object appError.
			return ''.
		end catch.

	end method.

	method public character DisplayFilterEntry():

		define variable cValue as character no-undo.

		if FilterRelation <> '' and
			FilterRelation <> ?
		then cValue = FilterRelation.

		if FieldName <> '' and
			FieldName <> ?
		then cValue = if cValue <> '' then substitute('&1 &2', cValue, substitute('&1 &2 &3', FieldName, Operator, FilterValue)) else substitute('&1 &2 &3', FieldName, Operator, FilterValue).
		else do:
			cValue = if cValue <> '' then substitute('&1 (', cValue) else '('.

			if valid-object(FilterObject)
			then cValue = cValue + FilterObject:DisplayQuery().

			cValue = substitute('&1 )', cValue).
		end.

		if valid-object(NextFilterObject)
		then cValue = substitute('&1 &2', cValue, NextFilterObject:DisplayFilterEntry()).

		return cValue.

		catch appError as Progress.Lang.Error :
			delete object appError.
			return ''.
		end catch.

	end method.

	destructor public __():
		delete object FilterObject		no-error.
		delete object NextFilterObject	no-error.
	end destructor.

end class.
