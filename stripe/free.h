/********************************************************************/
/*   STRIPE: converting a polygonal model to triangle strips    
     Francine Evans, 1996.
     SUNY @ Stony Brook
     Advisors: Steven Skiena and Amitabh Varshney


     Modified by Elvir Azanli, 1998
*/
/********************************************************************/

/*---------------------------------------------------------------------*/
/*   STRIPE: free.h
-----------------------------------------------------------------------*/

#ifndef FREE_INCLUDED
#define FREE_INCLUDED

#include "polverts.h"


void Free_Strips();
void End_Face_Struct(int numfaces);
void End_Edge_Struct(int numverts);

#endif
