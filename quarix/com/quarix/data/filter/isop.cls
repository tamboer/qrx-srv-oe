
/*------------------------------------------------------------------------
   File        : isop
   Purpose     :
   Syntax      :
   Description :
   Author(s)   : Adam
   Created     : Thu Jun 30 14:58:01 EEST 2011
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

using com.quarix.data.filter.operator.

class com.quarix.data.filter.isop:

	define private variable cOperator as character no-undo.

	constructor private isop():
	end constructor.

	constructor private isop(input pcOperator as character):
		cOperator = pcOperator.
	end constructor.

	define public static property EQ as com.quarix.data.filter.isop no-undo
		public get:
			if not valid-object(EQ) then
				assign EQ = new com.quarix.data.filter.isop(operator:isEq()).
			return EQ.
		end.
		private set.

	define public static property NE as com.quarix.data.filter.isop no-undo
		public get:
			if not valid-object(NE) then
				assign NE = new com.quarix.data.filter.isop(operator:isNe()).
			return NE.
		end.
		private set.

	define public static property LT as com.quarix.data.filter.isop no-undo
		public get:
			if not valid-object(LT) then
				assign LT = new com.quarix.data.filter.isop(operator:isLt()).
			return LT.
		end.
		private set.

	define public static property LE as com.quarix.data.filter.isop no-undo
		public get:
			if not valid-object(LE) then
				assign LE = new com.quarix.data.filter.isop(operator:isLe()).
			return LE.
		end.
		private set.

	define public static property GT as com.quarix.data.filter.isop no-undo
		public get:
			if not valid-object(GT) then
				assign GT = new com.quarix.data.filter.isop(operator:isGt()).
			return GT.
		end.
		private set.

	define public static property GE as com.quarix.data.filter.isop no-undo
		public get:
			if not valid-object(GE) then
				assign GE = new com.quarix.data.filter.isop(operator:isGe()).
			return GE.
		end.
		private set.

	define public static property MATCHES as com.quarix.data.filter.isop no-undo
		public get:
			if not valid-object(MATCHES) then
				assign MATCHES = new com.quarix.data.filter.isop(operator:isMatches()).
			return MATCHES.
		end.
		private set.

        define public static property BEGINS_ as com.quarix.data.filter.isop no-undo
            public get:
                if not valid-object(BEGINS_) then
                    assign BEGINS_ = new com.quarix.data.filter.isop(operator:isBegins()).
                return BEGINS_.
            end.
            private set.

	define public static property AND_ as com.quarix.data.filter.isop no-undo
		public get:
			if not valid-object(AND_) then
				assign AND_ = new com.quarix.data.filter.isop(operator:isAnd()).
			return AND_.
		end.
		private set.

	define public static property OR_ as com.quarix.data.filter.isop no-undo
		public get:
			if not valid-object(OR_) then
				assign OR_ = new com.quarix.data.filter.isop(operator:isOr()).
			return OR_.
		end.
		private set.

	method public override character ToString():
		return if cOperator = ? then "?" else cOperator.
	end method.

end class.
