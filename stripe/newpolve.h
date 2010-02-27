/********************************************************************/
/*   STRIPE: converting a polygonal model to triangle strips    
     Francine Evans, 1996.
     SUNY @ Stony Brook
     Advisors: Steven Skiena and Amitabh Varshney

     Modified by Elvir Azanli, 1998
*/
/********************************************************************/

/*---------------------------------------------------------------------*/
/*   STRIPE: newpolve.h                                                */
/*---------------------------------------------------------------------*/

#ifndef NEWPOLVE_INCLUDED
#define NEWPOLVE_INCLUDED

#include <stdio.h>
#include "global.h"
#include "polverts.h"



void Find_Bands(int numfaces, FILE *output_file, int *swaps, int *bands, 
                int *cost, int *tri, int norms, int *vert_norms,
                int texture, int *vert_texture);
void Save_Walks(int numfaces);
void Save_Rest(int *numfaces);
#endif

