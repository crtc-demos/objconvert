/********************************************************************/
/*   STRIPE: converting a polygonal model to triangle strips    
     Francine Evans, 1996.
     SUNY @ Stony Brook
     Advisors: Steven Skiena and Amitabh Varshney

     Modified by Elvir Azanli, 1998
*/
/********************************************************************/

/*---------------------------------------------------------------------*/
/*   STRIPE: util.h
-----------------------------------------------------------------------*/

#ifndef UTIL_INCLUDED
#define UTIL_INCLUDED

#include "polverts.h"

void switch_lower (int *x, int *y);
BOOL member(int x , int id1, int id2, int id3);
BOOL Exist(int face_id, int id1, int id2);
int Different (int id1,int id2,int id3,int id4,int id5,
               int id6, int *x, int *y);
int Return_Other(int *index,int e1,int e2);
int Get_Other_Vertex(int id1,int id2,int id3,int *index);
PLISTINFO Done(int face_id,  int *bucket);
void First_Edge(int *id1,int *id2, int *id3);
void Last_Edge(int *id1, int *id2, int *id3, BOOL save);

void preserve_strip_orientation_with_normal(FILE *output,
                                          int vertex1, int normal1,
                                          int vertex2, int normal2,
                                          int vertex3, int normal3);

void preserve_strip_orientation_with_texture(FILE *output,
                                          int vertex1, int texture1,
                                          int vertex2, int texture2,
                                          int vertex3, int texture3);

void  preserve_strip_orientation_with_texture_and_normal(FILE *output,
                                          int vertex1, int texture1, int normal1,
                                          int vertex2, int texture2, int normal2,
                                          int vertex3, int texture3, int normal3);

void preserve_strip_orientation(FILE *output,int vertex1, int vertex2,int vertex3);

#endif
