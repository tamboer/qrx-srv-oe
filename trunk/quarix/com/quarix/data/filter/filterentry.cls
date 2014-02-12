
 /*------------------------------------------------------------------------
    File        : filterentry
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

using Progress.Lang.*.
using com.quarix.data.filter.filterentry.
using com.quarix.data.operator.

class com.quarix.data.filter.filterentry:



/*
	define private variable FieldName       as character                          	no-undo.
	define private variable Operator        as character                          	no-undo.
	define private variable FilterValue     as character                          	no-undo.
	define private variable FilterObject	as com.quarix.data.filter.filterentry	no-undo.
	define private variable FilterObjectVal	as com.quarix.data.filter.filterentry	no-undo.

	constructor private filterentry():
	end constructor.

	method public logical _addCondition(input pcFieldName as character, input pcOperator as character, input pcFilterValue as character):

		if pcFieldName = ?		or
			pcFieldName = ''	or
			pcOperator = ?		or
			pcOperator = ''		or
			pcFilterValue = ?	or
			pcFilterValue = ''
			then return false.

		assign
			FieldName   = pcFieldName
			Operator    = pcOperator
			FilterValue	= pcFilterValue.

		return true.

		catch appError as Progress.Lang.Error :
			delete object appError.
			return false.
		end catch.

	end method.

	method public logical _addCondition(input oFilterObject as com.quarix.data.filter.filterentry, input pcOperator as character, input oFilterObjectVal as com.quarix.data.filter.filterentry):

		if not valid-object(oFilterObject)					or
			(pcOperator <> com.quarix.data.operator:isAnd() and
			pcOperator <> com.quarix.data.operator:isOr())	or
			not valid-object(oFilterObjectVal)
			then return false.

		assign
			FilterObject	= oFilterObject
			Operator		= pcOperator
			FilterObject	= oFilterObjectVal.

		return true.

		catch appError as Progress.Lang.Error :
			delete object appError.
			return false.
		end catch.

	end method.

	method static private filterentry addFilterCondition(input pcFieldName as character, input pcOperator as character, input pcFilterValue as character):

		define variable oFilterEntry as com.quarix.data.filter.filterentry no-undo.

		oFilterEntry = new com.quarix.data.filter.filterentry().

		if not oFilterEntry:_addCondition(input pcFieldName, input pcOperator, input pcFilterValue)
		then do:
			delete object oFilterEntry no-error.
			return ?.
		end.

		return oFilterEntry.

		catch appError as Progress.Lang.Error:
			delete object appError.
			delete object oFilterEntry no-error.
			return ?.
		end catch.

	end method.

	method static private filterentry addFilterCondition(input oFilterObject as com.quarix.data.filter.filterentry, input pcOperator as character, input oFilterObjectVal as com.quarix.data.filter.filterentry):

		define variable oFilterEntry as com.quarix.data.filter.filterentry no-undo.

		oFilterEntry = new com.quarix.data.filter.filterentry().

		if not oFilterEntry:_addCondition(input oFilterObject, input pcOperator, input oFilterObjectVal)
		then do:
			delete object oFilterEntry no-error.
			return ?.
		end.

		return oFilterEntry.

		catch appError as Progress.Lang.Error:
			delete object appError.
			delete object oFilterEntry no-error.
			return ?.
		end catch.

	end method.

	method static private filterentry addFilterCondition(input TTField as com.quarix.data.ttfield, input pcOperator as character, input pcFilterValue as character):
		return addFilterCondition(input TTField:getFqName(), input pcOperator, input pcFilterValue).
	end method.

	method static public filterentry isAnd(input oFilterObject as com.quarix.data.filter.filterentry, input oFilterObjectVal as com.quarix.data.filter.filterentry):
		return addFilterCondition(input oFilterObject, input com.quarix.data.operator:isAnd(), input oFilterObjectVal).
	end method.

	method static public filterentry isOr(input oFilterObject as com.quarix.data.filter.filterentry, input oFilterObjectVal as com.quarix.data.filter.filterentry):
		return addFilterCondition(input oFilterObject, input com.quarix.data.operator:isOr(), input oFilterObjectVal).
	end method.

	method static public filterentry isEq(input pcFieldName as character, input pcFilterValue as character):
		return addFilterCondition(input pcFieldName, input com.quarix.data.operator:isEq(), pcFilterValue).
	end method.

	method static public filterentry isEq(input TTField as com.quarix.data.ttfield, input pcFilterValue as character):
		return isEq(input TTField:getFqName(), input pcFilterValue).
	end method.

	method static public filterentry isNe(input pcFieldName as character, input pcFilterValue as character):
		return addFilterCondition(input pcFieldName, input com.quarix.data.operator:isNe(), pcFilterValue).
	end method.

	method static public filterentry isNe(input TTField as com.quarix.data.ttfield, input pcFilterValue as character):
		return isNe(input TTField:getFqName(), input pcFilterValue).
	end method.

	method static public filterentry isLt(input pcFieldName as character, input pcFilterValue as character):
		return addFilterCondition(input pcFieldName, input com.quarix.data.operator:isLt(), pcFilterValue).
	end method.

	method static public filterentry isLt(input TTField as com.quarix.data.ttfield, input pcFilterValue as character):
		return isLt(input TTField:getFqName(), input pcFilterValue).
	end method.

	method static public filterentry isLe(input pcFieldName as character, input pcFilterValue as character):
		return addFilterCondition(input pcFieldName, input com.quarix.data.operator:isLe(), pcFilterValue).
	end method.

	method static public filterentry isLe(input TTField as com.quarix.data.ttfield, input pcFilterValue as character):
		return isLe(input TTField:getFqName(), input pcFilterValue).
	end method.

	method static public filterentry isGt(input pcFieldName as character, input pcFilterValue as character):
		return addFilterCondition(input pcFieldName, input com.quarix.data.operator:isGt(), pcFilterValue).
	end method.

	method static public filterentry isGt(input TTField as com.quarix.data.ttfield, input pcFilterValue as character):
		return isGt(input TTField:getFqName(), input pcFilterValue).
	end method.

	method static public filterentry isGe(input pcFieldName as character, input pcFilterValue as character):
		return addFilterCondition(input pcFieldName, input com.quarix.data.operator:isGe(), pcFilterValue).
	end method.

	method static public filterentry isGe(input TTField as com.quarix.data.ttfield, input pcFilterValue as character):
		return isGe(input TTField:getFqName(), input pcFilterValue).
	end method.

	method static public filterentry isMatches(input pcFieldName as character, input pcFilterValue as character):
		return addFilterCondition(input pcFieldName, input com.quarix.data.operator:isMatches(), pcFilterValue).
	end method.

	method static public filterentry isMatches(input TTField as com.quarix.data.ttfield, input pcFilterValue as character):
		return isMatches(input TTField:getFqName(), input pcFilterValue).
	end method.

	method static public filterentry isBegins(input pcFieldName as character, input pcFilterValue as character):
		return addFilterCondition(input pcFieldName, input com.quarix.data.operator:isBegins(), pcFilterValue).
	end method.

	method static public filterentry isBegins(input TTField as com.quarix.data.ttfield, input pcFilterValue as character):
		return isBegins(input TTField:getFqName(), input pcFilterValue).
	end method.

	destructor public filterentry():
		delete object FilterObject		no-error.
		delete object FilterObjectVal	no-error.
	end method.
*/
end class.
