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
* $Revision: 10610 $
* $Id: wxsAxis.h 10610 2015-12-07 05:32:34Z jenslody $
* $HeadURL: svn://svn.code.sf.net/p/codeblocks/code/branches/release-20.xx/src/plugins/contrib/wxSmithContribItems/wxSmithPlot/wxsAxis.h $
*/

#ifndef WXSSTATICTEXT1_H
#define WXSSTATICTEXT1_H

#include    <wxwidgets/wxswidget.h>

#include    <wx/stattext.h>
#include    <mathplot.h>

/** \brief Class for wxsStaticText widget */
class wxsAxis: public wxsWidget
{
    public:

        wxsAxis(wxsItemResData* Data);

    private:

        virtual void      OnBuildCreatingCode();
        virtual wxObject* OnBuildPreview(wxWindow* Parent,long Flags);
        virtual void      OnBuildDeclarationsCode();
        virtual void      OnEnumWidgetProperties(long Flags);

        long            mType;              // 0=X-axis, 1=Y-axis
        wxString        mLabel;             // label the axis
        long            mAlign;             // position the axis
        bool            mTics;              // show tic marks
        wxsColourData   mPenColour;         // color to draw
        wxsFontData     mPenFont;           // for drawing the text

};

#endif
