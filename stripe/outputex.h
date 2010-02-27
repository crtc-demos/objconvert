/********************************************************************/
/*   STRIPE: converting a polygonal model to triangle strips    
     Francine Evans, 1996.
     SUNY @ Stony Brook
     Advisors: Steven Skiena and Amitabh Varshney

     Modified by Elvir Azanli, 1998
*/
/********************************************************************/

/*---------------------------------------------------------------------*/
/*   STRIPE: outputex.h
-----------------------------------------------------------------------*/

#ifndef OUTPUTEX_INCLUDED
#define OUTPUTEX_INCLUDED

#include <stdio.h>
#include "polverts.h"
#include "queue.h"

#define TRIANGLE 3
#define MAGNITUDE 1000000

void Output_TriEx(int id1, int id2, int id3, FILE *output, 
                  int flag,int where);
void Extend_BackwardsEx(int face_id, FILE *output, int *ties, 
                        int tie, int triangulate, 
                        int swaps,int *next_id);
void Polygon_OutputEx(P_ADJACENCIES temp,int face_id,int bucket,
                      ListHead *pListHead, FILE *output,
                      int *ties, int tie,int triangulate, int swaps,
                      int *next_id, int where);

#endif
