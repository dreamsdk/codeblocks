#include "farrays.h"

void ClearPassedTokensArray2D(PassedTokensArray2D &array)
{
    for(size_t j=0; j<array.size(); j++)
    {
        TokensArrayFlat* tf = array[j];
        for(size_t i=0; i<tf->size(); i++)
        {
            tf->Item(i)->Clear();
            delete tf->Item(i);
        }
        delete tf;
    }
    array.clear();
}

void ClearArrOfSizeT2D(ArrOfSizeT2D &array)
{
    for(size_t i=0; i<array.size(); i++)
    {
        delete array[i];
    }
    array.clear();
}

void ClearBoolArray3D(BoolArray3D &array)
{
    for(size_t k=0; k<array.size(); k++)
    {
        BoolArray2D* ba2d = array[k];
        for(size_t j=0; j<ba2d->size(); j++)
        {
            delete ba2d->at(j);
        }
        delete ba2d;
    }
    array.clear();
}

void ClearBoolArray2D(BoolArray2D &array)
{
    for(size_t j=0; j<array.size(); j++)
    {
        delete array[j];
    }
    array.clear();
}

