/*
* This file is part of wxSmith plugin for Code::Blocks Studio
* Copyright (C) 2006-2007  Bartlomiej Swiecki
*
* wxSmith is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation; either version 3 of the License, or
* (at your option) any later version.
*
* wxSmith is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with wxSmith. If not, see <http://www.gnu.org/licenses/>.
*
* $Revision: 13547 $
* $Id: wxstoolbar.h 13547 2024-09-14 04:35:04Z mortenmacfly $
* $HeadURL: https://svn.code.sf.net/p/codeblocks/code/branches/release-25.03/src/plugins/contrib/wxSmith/wxwidgets/defitems/wxstoolbar.h $
*/

#ifndef WXSTOOLBAR_H
#define WXSTOOLBAR_H

#include "../wxstool.h"

/** \brief Class represening wxToolBar */
class wxsToolBar: public wxsTool
{
    public:

        wxsToolBar(wxsItemResData* Data);

    protected:

        virtual void OnBuildCreatingCode();
        virtual void OnEnumToolProperties(long _Flags);
        virtual bool OnIsPointer() { return true; }
        virtual bool OnCanAddToResource(wxsItemResData* Data,bool ShowMessage);
        virtual bool OnCanAddChild(wxsItem* Item,bool ShowMessage);
        virtual bool OnMouseDClick(wxWindow* Preview,int PosX,int PosY);
        virtual bool OnXmlReadChild(TiXmlElement* Elem,bool IsXRC,bool IsExtra);

        wxsSizeData m_BitmapSize;
        wxsSizeData m_Margins;
        long        m_Packing;
        long        m_Separation;

        friend class wxsToolBarEditor;
};



#endif
