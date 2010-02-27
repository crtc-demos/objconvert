/********************************************************************/
/*   STRIPE: converting a polygonal model to triangle strips    
     Francine Evans, 1996.
     SUNY @ Stony Brook
     Advisors: Steven Skiena and Amitabh Varshney

     Modified by Elvir Azanli, 1998
*/
/********************************************************************/

/*---------------------------------------------------------------------*/
/*   STRIPE: common.h
-----------------------------------------------------------------------*/

#ifndef COMMON_INCLUDED
#define COMMON_INCLUDED

#include "global.h"

int  Old_Adj(int face_id);
int Number_Adj(int id1, int id2, int curr_id);
int Min_Adj(int id);
void Check_In_Polygon(int face_id, int *min, int size);
void New_Face (int face_id, int v1, int v2, int v3);
void New_Size_Face (int face_id);
void Check_In_Quad(int face_id,int *min);
int Get_Output_Edge(int face_id, int size, int *index,int id2,int id3);
void Get_Input_Edge(int *index,int id1,int id2,int id3,int *new1,int *new2,
                    int size,int face_id);
int Find_Face(int current_face, int id1, int id2, int *bucket);
BOOL Look_Up(int id1,int id2,int face_id);
void Add_Id_Strips(int id, int where);
int Num_Adj(int id1, int id2);
void Add_Sgi_Adj(int bucket,int face_id);
void Find_Adjacencies(int num_faces);

#endif
