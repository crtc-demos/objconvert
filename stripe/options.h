/********************************************************************/
/*   STRIPE: converting a polygonal model to triangle strips    
     Francine Evans, 1996.
     SUNY @ Stony Brook
     Advisors: Steven Skiena and Amitabh Varshney

     Modified by Elvir Azanli, 1998
*/
/********************************************************************/

/*---------------------------------------------------------------------*/
/*   STRIPE: options.h
-----------------------------------------------------------------------*/


#ifndef OPTIONS_INCLUDED
#define OPTIONS_INCLUDED

enum file_options {ASCII,BINARY};
enum tie_options {FIRST, RANDOM, ALTERNATE, LOOK, SEQUENTIAL};
enum triangulation_options {PARTIAL,WHOLE};

void print_usage(void);
float get_options(int argc, char **argv, int *f, int *t, int *tr, int *group,
		  int *orientation);

#endif
     
