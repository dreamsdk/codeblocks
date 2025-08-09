/*
 * This file is part of the Code::Blocks IDE and licensed under the GNU Lesser General Public License, version 3
 * http://www.gnu.org/licenses/lgpl-3.0.html
 *
 * $Revision: 7109 $
 * $Id: ModPoller.cpp 7109 2011-04-15 11:53:16Z mortenmacfly $
 * $HeadURL: https://svn.code.sf.net/p/codeblocks/code/branches/release-25.03/src/plugins/modpoller/ModPoller.cpp $
 */

#include <sdk.h> // Code::Blocks SDK
#include <configurationpanel.h>
#include "ModPoller.h"

namespace
{
    PluginRegistrant<ModPoller> reg(_T("ModPoller"));
}
