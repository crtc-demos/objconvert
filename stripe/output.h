/********************************************************************/
/*   STRIPE: converting a polygonal model to triangle strips    
     Francine Evans, 1996.
     SUNY @ Stony Brook
     Advisors: Steven Skiena and Amitabh Varshney

     Modified by Elvir Azanli, 1998
*/
/********************************************************************/

/*---------------------------------------------------------------------*/
/*   STRIPE: output.h
-----------------------------------------------------------------------*/


#ifndef OUTPUT_INCLUDED
#define OUTPUT_INCLUDED

#define TRIANGLE 3
#define MAGNITUDE 1000000

#include <stdio.h>
#include "global.h"

int Finished(int *swap, FILE *output, int startnewstrip);
void Output_Tri(int id1, int id2, int id3,BOOL end);
void Set_Norms (int *vert_norm, int normals, int *vert_texture, int texture);
int Extend_Face(int face_id,int e1,int e2,int *swaps,FILE *bands,
                int color1,int color2,int color3,int *vert_norm, int normals,
                int *vert_texture, int texture);
#endif
