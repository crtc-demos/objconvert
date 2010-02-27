/********************************************************************/
/*   STRIPE: converting a polygonal model to triangle strips    
     Francine Evans, 1996.
     SUNY @ Stony Brook
     Advisors: Steven Skiena and Amitabh Varshney

     Modified by Elvir Azanli, 1998
*/
/********************************************************************/

/*---------------------------------------------------------------------*/
/*   STRIPE:local.h
-----------------------------------------------------------------------*/
#ifndef LOCAL_INCLUDED
#define LOCAL_INCLUDED

#include <stdio.h>


void SGI_Strip(int num_faces,FILE *output,
	       int ties,int triangulate);


#endif
