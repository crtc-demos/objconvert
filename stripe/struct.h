/********************************************************************/
/*   STRIPE: converting a polygonal model to triangle strips
     Francine Evans, 1996.
     SUNY @ Stony Brook
     Advisors: Steven Skiena and Amitabh Varshney

     Modified by Elvir Azanli, 1998
*/
/********************************************************************/

/*---------------------------------------------------------------------*/
/*   STRIPE: struct.h
*/
/*---------------------------------------------------------------------*/

#ifndef STRUCT_INCLUDED
#define STRUCT_INCLUDED

#include "polverts.h"
#include "queue.h"

int Get_Edge(int *edge1,int *edge2,int *index,int face_id,
             int size, int id1, int id2);
int Update_Adjacencies(int face_id,int *next_bucket,int *e1,int *e2,int *ties);

#endif
