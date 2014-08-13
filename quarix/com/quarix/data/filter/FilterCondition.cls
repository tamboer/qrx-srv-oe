/*------------------------------------------------------------------------
File : FilterCondition
Purpose : Filtering conditions for coded filters
Syntax :
Description :
Author(s) : tomd
Created : Thu Aug 12 13:55:00 CET 2014
Notes :
License :
This file is part of the QRX-SRV-OE software framework.

The QRX-SRV-OE software framework is free software; you can redistribute
it and/or modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either version 2.1
of the License, or (at your option) any later version.

The QRX-SRV-OE software framework is distributed in the hope that it will
be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the QRX-SRV-OE software framework; if not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA or on the internet at the following address:
http://www.gnu.org/licenses/lgpl-2.1.txt
----------------------------------------------------------------------*/

using com.quarix.data.filter.*.

class com.quarix.data.filter.FilterCondition:

  def private var cValue as char no-undo.

  constructor protected FilterCondition(input icValue as char):
     assign cValue = icValue.
  end constructor.


  def public static property EQ as FilterCondition no-undo
  public get:
    if not valid-object(EQ) then 
       assign EQ = new FilterCondition('EQ').
    return EQ.
  end.
  private set.

  def public static property Equal as FilterCondition no-undo
  public get:
    return EQ.
  end.
  private set.

  def public static property GT as FilterCondition no-undo
  public get:
    if not valid-object(GT) then
      assign GT = new FilterCondition('GT').
    return GT.
  end.
  private set.
  
  def public static property GreaterThan as FilterCondition no-undo
  public get:
    return GT.
  end.
  private set.

  def public static property GE as FilterCondition no-undo
  public get:
    if not valid-object(GE) then
      assign GE = new FilterCondition('GE').
    return GE.
  end.
  private set.
  
  def public static property GreaterOrEqual as FilterCondition no-undo
  public get:
    return GE.
  end.
  private set.  
  
  def public static property LT as FilterCondition no-undo
  public get:
    if not valid-object(GE) then
      assign GE = new FilterCondition('LT').
    return GE.
  end.
  private set.
  
  def public static property LessThan as FilterCondition no-undo
  public get:
    return LT.
  end.
  private set.

  def public static property LE as FilterCondition no-undo
  public get:
    if not valid-object(LE) then
      assign LE = new FilterCondition('LE').
    return LE.
  end.
  private set.
  
  def public static property LessOrEqual as FilterCondition no-undo
  public get:
    return LE.
  end.
  private set.

  def public static property NE as FilterCondition no-undo
  public get:
    if not valid-object(NE) then
      assign NE = new FilterCondition('NE').
    return NE.
  end.
  private set.
  
  def public static property NotEqual as FilterCondition no-undo
  public get:
    return NE.
  end.
  private set.
  
  def public static property BeginsWith as FilterCondition no-undo
  public get:
    if not valid-object(BeginsWith) then
      assign BeginsWith = new FilterCondition('Begins').
    return BeginsWith.
  end.
  private set.
  
  def public static property Begins as FilterCondition no-undo
  public get:
    return BeginsWith.
  end.
  private set.
  
  def public static property Matches as FilterCondition no-undo
  public get:
    if not valid-object(Matches) then
      assign Matches = new FilterCondition('Matches').
    return Matches.
  end.
  private set.
  


  method public override char ToString():
    return if cValue = ? then "?" else cValue.
  end method. 



end class.
