/*
 * This file is part of the Code::Blocks IDE and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 * $Revision: 7754 $
 * $Id: app.cpp 7754 2012-02-01 09:59:56Z mortenmacfly $
 * $HeadURL: https://svn.code.sf.net/p/codeblocks/code/branches/release-25.03/src/tools/cb_share_config/app.cpp $
 */

#include "app.h"

//(*AppHeaders
#include "mainframe.h"
#include <wx/image.h>
//*)

IMPLEMENT_APP(MyApp);

bool MyApp::OnInit()
{

	//(*AppInitialize
	bool wxsOK = true;
	wxInitAllImageHandlers();
	if ( wxsOK )
	{
		MainFrame* Frame = new MainFrame(0);
		Frame->Show();
		SetTopWindow(Frame);
	}
	//*)

	return wxsOK;
}
