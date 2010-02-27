/********************************************************************/
/*   STRIPE: converting a polygonal model to triangle strips    
     Francine Evans, 1996.
     SUNY @ Stony Brook
     Advisors: Steven Skiena and Amitabh Varshney

     Modified by Elvir Azanli, 1998
*/
/********************************************************************/

/*---------------------------------------------------------------------*/
/*   STRIPE: partial.h
-----------------------------------------------------------------------*/

#ifndef PARTIAL_INCLUDED
#define PARTIAL_INCLUDED

#include <stdio.h>
#include "polverts.h"
#include "queue.h"



void Partial_Triangulate(int size,int *index,
                         FILE *output,int next_face_id,int face_id,
                         int *next_id,ListHead *pListHead,
                         P_ADJACENCIES temp, int where);


void Inside_Polygon(int size,int *index,
                    int face_id,ListHead *pListHead, int where);

#endif
