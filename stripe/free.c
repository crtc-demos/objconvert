/********************************************************************/
/*   STRIPE: converting a polygonal model to triangle strips    
     Francine Evans, 1996.
     SUNY @ Stony Brook
     Advisors: Steven Skiena and Amitabh Varshney
     
     Modified by Elvir Azanli, 1998
*/
/********************************************************************/

/*---------------------------------------------------------------------*/
/*   STRIPE: free.c
     This file contains the code used to free the data structures.
*/
/*---------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include "free.h"
#include "polverts.h"

static void ParseAndFreeList( ListHead *pListHead )
{
  PLISTINFO value;
  register int c,num;
  
  /*    Freeing a linked list */
  num = NumOnList(pListHead);
  for (c = 0; c< num; c++)
    value =   RemHead(pListHead);
} 



void Free_Strips()
{
  /*    Free strips data structure */
  if (strips[0] == NULL)
    return;
  else
    ParseAndFreeList(strips[0]);
}


static void FreeFaceTable(int nSize)
{
  register int nIndex;
  
  for ( nIndex=0; nIndex < nSize; nIndex++ )
    { 
      if ( PolFaces[nIndex] != NULL ) 
	ParseAndFreeList( PolFaces[nIndex] );
    }
  free( PolFaces );
}

static void FreeEdgeTable(int nSize)
{
  register int nIndex;
  
  for ( nIndex=0; nIndex < nSize; nIndex++ )
    {
      if ( PolEdges[nIndex] != NULL )
	ParseAndFreeList( PolEdges[nIndex] );
    }
  free( PolEdges );
}



void End_Face_Struct(int numfaces)
{
  FreeFaceTable(numfaces);
}

void End_Edge_Struct(int numverts)
{
  FreeEdgeTable(numverts);
}
