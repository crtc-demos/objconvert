/********************************************************************/
/*   STRIPE: converting a polygonal model to triangle strips    
     Francine Evans, 1996.
     SUNY @ Stony Brook
     Advisors: Steven Skiena and Amitabh Varshney

     Modified by Elvir Azanli, 1998
*/
/********************************************************************/

/*---------------------------------------------------------------------*/
/*   STRIPE: structex.h
*/
/*---------------------------------------------------------------------*/


#ifndef STRUCTEX_INCLUDED
#define STRUCTEX_INCLUDED
#include "polverts.h"
#include "queue.h"

int Get_EdgeEx(int *edge1,int *edge2,int *index,int face_id,
	       int size, int id1, int id2);
void Delete_AdjEx(int id1, int id2,int *next_bucket,int *min_face, 
		  int current_face,int *e1,int *e2,int *ties);
int Change_FaceEx(int face_id,int in1,int in2,
		  ListHead *pListHead, BOOL no_check);
int Update_AdjacenciesEx(int face_id, int *next_bucket, int *e1, int *e2,
			 int *ties);
int Min_Face_AdjEx(int face_id, int *next_bucket, int *ties);


#endif
