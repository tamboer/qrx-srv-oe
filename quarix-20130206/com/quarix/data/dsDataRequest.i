/*------------------------------------------------------------------------
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

/* standard xml data request wpf&ajax */
&if defined(quarix-standard-xml) gt 0 &then
define {&scope} temp-table dsMsg no-undo {&reference-only}
   field command         as character xml-node-type 'attribute':u
   field responseFormat  as character xml-node-type 'attribute':u
   field batchSize       as integer   xml-node-type 'attribute':u
   field dataBody        as clob.

define {&scope} temp-table tblMsg no-undo {&reference-only}
   field id               as character xml-node-type 'attribute':u
   field startRowId       as character xml-node-type 'attribute':u
   field startRow         as integer   xml-node-type 'attribute':u
   field batchSize        as integer   xml-node-type 'attribute':u
   field skipRow          as logical   xml-node-type 'attribute':u
   field prefetch         as logical   xml-node-type 'attribute':u
   index pktblMsg         is primary id.

define {&scope} temp-table filter no-undo {&reference-only}
   field fld      as character xml-node-type 'attribute':u
   field op       as character xml-node-type 'attribute':u
   field val      as character xml-node-type 'attribute':u
   field daoId    as character xml-node-type 'hidden':u
   index pktblFilter is primary daoId fld.

define {&scope} temp-table srch no-undo {&reference-only}
    field fld      as character xml-node-type 'attribute':u
    field val      as character xml-node-type 'attribute':u
    field daoId    as character xml-node-type 'hidden':u
    index pktblSearch is primary daoId fld.

define {&scope} temp-table sort no-undo {&reference-only}
   field fld      as character xml-node-type 'attribute':u
   field rev      as logical   xml-node-type 'attribute':u
   field daoId    as character xml-node-type 'hidden':u
   index pktblSort is primary daoId fld.


define {&scope} dataset reqMsg {&reference-only} for
   dsMsg, tblMsg, filter, srch, sort
   data-relation daoFilter for tblMsg, filter
       relation-fields (tblMsg.id, filter.daoId) nested
   data-relation daoSearch for tblMsg, srch
        relation-fields (tblMsg.id, srch.daoId) nested
   data-relation daoSort  for tblMsg, sort
       relation-fields (tblMsg.id, sort.daoId) nested.

/* initial data request - ajax */
&else
   define {&scope} temp-table action no-undo {&reference-only}
    field command         as character xml-node-type 'attribute':u
    field responseFormat  as character xml-node-type 'attribute':u.

   define {&scope} temp-table daoMsg no-undo {&reference-only}
    field id               as character xml-node-type 'attribute':u
    field command          as character xml-node-type 'attribute':u
    field responseFormat   as character xml-node-type 'attribute':u
    field startRowId       as character xml-node-type 'attribute':u
    field startRow         as integer   xml-node-type 'attribute':u
    field batchSize        as integer   xml-node-type 'attribute':u
    field skipRow          as logical   xml-node-type 'attribute':u
    field prefetch         as logical   xml-node-type 'attribute':u
    index pkdaoMsg   is primary id.

   define {&scope} temp-table filter no-undo {&reference-only}
    field fld      as character xml-node-type 'attribute':u
    field op       as character xml-node-type 'attribute':u
    field val      as character xml-node-type 'attribute':u
    field daoId    as character xml-node-type 'hidden':u
    index pkdaoFilter is primary daoId fld.

   define {&scope} temp-table srch no-undo {&reference-only}
    field fld      as character xml-node-type 'attribute':u
    field val      as character xml-node-type 'attribute':u
    field daoId    as character xml-node-type 'hidden':u
    index pkdaoSearch is primary daoId fld.

   define {&scope} temp-table sort no-undo {&reference-only}
    field fld      as character xml-node-type 'attribute':u
    field rev      as logical   xml-node-type 'attribute':u
    field daoId    as character xml-node-type 'hidden':u
    index pkdaoSort is primary daoId fld.

   define {&scope} temp-table row no-undo {&reference-only}
    field id       as character xml-node-type 'attribute':u
    field state    as integer   xml-node-type 'attribute':u
    field daoId    as character xml-node-type 'hidden':u
    index pkdaoSort is primary daoId id.

   define {&scope} temp-table rowImage no-undo {&reference-only}
    field type     as character xml-node-type 'attribute':u
    field daoId    as character xml-node-type 'hidden':u
    field id       as character xml-node-type 'hidden':u
    index pkdaoRowImage is primary daoId id type.

   define {&scope} temp-table fld no-undo {&reference-only}
    field name     as character xml-node-type 'attribute':u
    field val      as clob      xml-node-type 'text':u   column-codepage 'utf-8':u
    field daoId    as character xml-node-type 'hidden':u
    field id       as character xml-node-type 'hidden':u
    field type     as character xml-node-type 'hidden':u
    index pkdaoRowData is primary daoId id type name.


   define {&scope} dataset datasetMsg {&reference-only} for
    action, daoMsg, filter, srch, sort, row, rowImage, fld
    data-relation daoFilter for daomsg, filter
        relation-fields (daoMsg.id, filter.daoId) nested
    data-relation daoSearch for daomsg, srch
        relation-fields (daoMsg.id, srch.daoId) nested
    data-relation daoSort  for daoMsg, sort
        relation-fields (daoMsg.id, sort.daoId) nested
    data-relation daoRow  for daoMsg, row
        relation-fields (daoMsg.id, row.daoId) nested
    data-relation daoRowImg  for row, rowImage
        relation-fields (row.daoId, rowImage.daoid, row.id, rowImage.id) nested
    data-relation daoRowData  for rowImage, fld
        relation-fields (rowImage.daoid, fld.daoId, rowImage.id, fld.id, rowImage.type, fld.type) nested.

&endif


