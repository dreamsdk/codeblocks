/*
 * This file is part of the FortranProject plugin for Code::Blocks IDE
 * and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 */

#ifndef FARRAYS_H
#define FARRAYS_H

#include <wx/dynarray.h>

#include "tokenf.h"
#include <vector>

typedef std::vector<TokensArrayFlat*> PassedTokensArray2D;
typedef std::vector<ArrOfSizeT*> ArrOfSizeT2D;
typedef std::vector<bool> BoolArray1D;
typedef std::vector<BoolArray1D*> BoolArray2D;
typedef std::vector<BoolArray2D*> BoolArray3D;

void ClearPassedTokensArray2D(PassedTokensArray2D &array);
void ClearArrOfSizeT2D(ArrOfSizeT2D &array);
void ClearBoolArray3D(BoolArray3D &array);
void ClearBoolArray2D(BoolArray2D &array);

#endif // FARRAYS_H
