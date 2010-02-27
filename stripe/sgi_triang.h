/********************************************************************/
/*   STRIPE: converting a polygonal model to triangle strips    
     Francine Evans, 1996.
     SUNY @ Stony Brook
     Advisors: Steven Skiena and Amitabh Varshney

     Modified by Elvir Azanli, 1998
*/
/********************************************************************/

/*---------------------------------------------------------------------*/
/*   STRIPE: sgi_triang.h
*/
/*---------------------------------------------------------------------*/


#ifndef SGI_TRIANG_INCLUDED
#define SGI_TRIANG_INCLUDED

#include <stdio.h>
#include "global.h"

int Adjacent(int id2,int id1, int *list, int size);
void Build_SGI_Table(int num_faces);
void Non_Blind_Triangulate(int size,int *index,
			   FILE *output,int next_face_id,int face_id,int where,
                           int color1,int color2,int color3);
void Blind_Triangulate(int size, int *index, BOOL begin, int where);


#endif
