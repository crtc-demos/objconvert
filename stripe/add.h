/********************************************************************/
/*   STRIPE: converting a polygonal model to triangle strips    
     Francine Evans, 1996.
     SUNY @ Stony Brook
     Advisors: Steven Skiena and Amitabh Varshney

     Modified by Elvir Azanli, 1998
*/
/********************************************************************/

/*---------------------------------------------------------------------*/
/*   STRIPE: add.h
-----------------------------------------------------------------------*/
#ifndef ADD_INCLUDED
#define ADD_INCLUDED

#include "global.h"

BOOL norm_array(int id, int vertex, double normal_difference,
                struct vert_struct *n, int num_vert);
void add_texture(int id,BOOL vertex);
int  add_vert_id(int id, int index_count);
void add_norm_id(int id, int index_count);
void AddNewFace(int ids[MAX1], int vert_count, int face_id, int norms[MAX1]);
void CopyFace(int ids[MAX1], int vert_count, int face_id, int norms[MAX1]);
void Add_AdjEdge(int v1,int v2,int fnum,int index1 );

#endif
