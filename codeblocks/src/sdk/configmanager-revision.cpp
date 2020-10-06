/*
 * This file is part of the Code::Blocks IDE and licensed under the GNU Lesser General Public License, version 3
 * http://www.gnu.org/licenses/lgpl-3.0.html
 *
 * $Revision: 10907 $
 * $Id: configmanager-revision.cpp 10907 2016-09-25 16:09:48Z fuscated $
 * $HeadURL: svn://svn.code.sf.net/p/codeblocks/code/branches/release-20.xx/src/sdk/configmanager-revision.cpp $
 */

/* ------------------------------------------------------------------------------------------------------------------
*  Decouple the autorevisioning code, so we don't need to recompile the whole manager each time.
*  It's bad enough we have to re-link the SDK...
*/

#include "sdk_precomp.h"

#ifndef CB_PRECOMP
    #include "configmanager.h"
    #include <wx/string.h>
#endif

#include "autorevision.h"

wxString ConfigManager::GetRevisionString()
{
    static_assert(wxMinimumVersion<2,8,12>::eval, "wxWidgets 2.8.12 is required");

    return autorevision::svnRevision;
}

unsigned int ConfigManager::GetRevisionNumber()
{
    return autorevision::svn_revision;
}

wxString ConfigManager::GetSvnDate()
{
    return autorevision::svnDate;
}

