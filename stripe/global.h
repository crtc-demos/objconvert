/********************************************************************/
/*   STRIPE: converting a polygonal model to triangle strips    
     Francine Evans, 1996.
     SUNY @ Stony Brook
     Advisors: Steven Skiena and Amitabh Varshney

     Modified by Elvir Azanli, 1998
*/
/********************************************************************/

/*---------------------------------------------------------------------*/
/*   STRIPE: global.h
-----------------------------------------------------------------------*/

#ifndef GLOBAL_INCLUDED 
#define GLOBAL_INCLUDED

#include "polverts.h"

enum swap_type {ON,OFF};

#define   VRDATA		double
#define   MAX1            60
	
#define	TRUE	      	1
#define	FALSE		0

#define   PI	     	3.1415926573
#define   ATOI(C)        (C -'0')
#define   X              0
#define   Y              1
#define   Z              2
#define   EVEN(x)       (((x) & 1) == 0)
#define   MAX_BAND      10000

struct vert_struct {
	VRDATA	x, y, z;	/* point coordinates */
};

int     ids[MAX1];
int     norms[MAX1];
int     *vert_norms;
int     *vert_texture;
#endif
