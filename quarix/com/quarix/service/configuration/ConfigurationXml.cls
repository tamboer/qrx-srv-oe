/*---------------------------------------------------------------------
File        : ConfigurationCore.cls
Purpose     :

Syntax      :

Description :

Author(s)   : Marian
Created     : generated 10/12/08
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
---------------------------------------------------------------------*/
routine-level on error undo, throw.

using com.quarix.system.FileSystem.

class com.quarix.service.configuration.ConfigurationXml
   inherits com.quarix.service.configuration.ConfigurationBase
   implements com.quarix.base.iSingleton use-widget-pool:

   define private variable fileSystemImp as FileSystem no-undo.

   define private property fileSystem    as FileSystem no-undo
      get:
         if not valid-object(fileSystemImp) then
            fileSystemImp = cast(GetInstance('com.quarix.system.FileSystem':u), 'com.quarix.system.FileSystem':u).
         return fileSystemImp.
      end get.


   define private temp-table ttSection no-undo   xml-node-name 'section':u
      field sectionName as character xml-node-type 'attribute':u xml-node-name 'name':u
      index pk is primary unique sectionName.

   define private temp-table ttKey   no-undo   xml-node-name 'key':u
      field sectionName as character xml-node-type 'hidden':u
      field objectName  as character xml-node-type 'attribute':u xml-node-name 'object':u
      field keyName     as character xml-node-type 'attribute':u xml-node-name 'name':u
      field keyVal      as character xml-node-type 'text':u
      index pk is primary unique sectionName objectName keyName.

   define private dataset dsConfig xml-node-name 'config':u
      for ttSection, ttKey
      data-relation for ttSection, ttKey nested
      relation-fields (sectionName, sectionName).

   constructor public ConfigurationXml():
      assign
         PassThrough = false
         MergeOnSave = false.
   end constructor.

   destructor public ConfigurationXml():
      UnloadInstance(fileSystemImp).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end destructor.

   method override protected logical LoadEnvironment (input environmentName as character):
      define variable retVal     as  logical    no-undo.
      define buffer   ttKey      for ttKey.

      if Util:IsEmpty(environmentName) then
         return false.

   	  if valid-object(FileSystem) and FileSystem:CanRead(environmentName) then
   	     retVal = dataset dsConfig:read-xml('file':u, FileSystem:GetFullPath(environmentName), 'empty':u, ?, true).

      if retVal then do:
         Environment = environmentName.
         for each ttKey
            on error undo, throw:
            SetKey (ttKey.keyName, ttKey.keyVal, ttKey.sectionName, ttKey.objectName).
         end.
      end.

   	  return retVal.

   	catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
      finally:
         dataset dsConfig:empty-dataset().
      end finally.
   end method.

   method override public logical SaveEnvironment (input environmentName as character):
      define variable retVal     as logical    no-undo.
      define buffer   ttKey      for ttKey.
      define buffer   ttSection  for ttSection.

      if not valid-object(FileSystem) or
         not FileSystem:CanWrite(environmentName) or
         not super:SaveEnvironment(environmentName) then
         return false.

      for each ttKey
         group by ttKey.sectionName
            on error undo, throw:
         if first-of (ttKey.sectionName) then do:
            create ttSection.
            ttSection.sectionName = ttKey.sectionName.
         end.
      end.

   	  retVal = dataset dsConfig:write-xml('file':u, FileSystem:GetFullPath(environmentName), true, 'utf-8', ?, false).

       return retVal.

   	catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
      finally:
         dataset dsConfig:empty-dataset().
      end finally.
   end method.

   method protected override logical SaveKey (input keyName as character, input keyValue as character, input sectionName as character, input objectName as character):
      define buffer   ttKey      for ttKey.

      for each ttKey
         where ttKey.sectionName eq sectionName
           and ttKey.objectName  eq objectName
           and ttKey.keyName     eq keyName
              on error undo, throw:
         ttKey.keyVal = keyValue.
         return true.
      end.

      create ttKey.
      assign
         ttKey.sectionName = sectionName
         ttKey.objectName  = objectName
         ttKey.keyName     = keyName
         ttKey.keyVal      = keyValue.
      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public void Reset():
   end method.
end class.


