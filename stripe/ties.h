/********************************************************************/
/*   STRIPE: converting a polygonal model to triangle strips    
     Francine Evans, 1996.
     SUNY @ Stony Brook
     Advisors: Steven Skiena and Amitabh Varshney

     Modified by Elvir Azanli, 1998
*/
/********************************************************************/

/*---------------------------------------------------------------------*/
/*   STRIPE: ties.h
-----------------------------------------------------------------------*/

#ifndef TIES_INCLUDED
#define TIES_INCLUDED

void Clear_Ties();
void Add_Ties(int id);
int Get_Next_Face(int t, int face_id, int triangulate);

#endif

