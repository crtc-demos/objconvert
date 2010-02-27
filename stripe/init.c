/********************************************************************/
/*   STRIPE: converting a polygonal model to triangle strips    
     Francine Evans, 1996.
     SUNY @ Stony Brook
     Advisors: Steven Skiena and Amitabh Varshney
     
     Modified by Elvir Azanli, 1998
*/
/********************************************************************/

/*---------------------------------------------------------------------*/
/*   STRIPE: init.c
     This file contains the initialization of data structures.
*/
/*---------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include "init.h"
#include "global.h"
#include "polverts.h"
#include "queue.h"

void init_vert_norms(int num_vert)
{
  /*   Initialize vertex/normal array to have all zeros to
       start with.
  */
  register int x;
  
  for (x = 0; x < num_vert; x++)
    *(vert_norms + x) = 0;
}

void init_vert_texture(int num_vert)
{
  /*   Initialize vertex/normal array to have all zeros to
       start with.
  */
  register int x;
  
  for (x = 0; x < num_vert; x++)
    *(vert_texture + x) = 0;
}

static BOOL InitVertexTable( int nSize )
{
  register int nIndex;
  
  /*     Initialize the face table */
  Vertices = (ListHead**) malloc(sizeof(ListHead*) * nSize ); 
  
  if ( Vertices )
    {
      for ( nIndex=0; nIndex < nSize; nIndex++ )
	{
	  Vertices[nIndex] = NULL;
	}
      return( TRUE );
    }
  return( FALSE );
} 

static BOOL InitFaceTable( int nSize )
{
  register int nIndex;
  
  /*     Initialize the face table */
  PolFaces = (ListHead**) malloc(sizeof(ListHead*) * nSize ); 
  
  if ( PolFaces )
    {
      for ( nIndex=0; nIndex < nSize; nIndex++ )
	{
	  PolFaces[nIndex] = NULL;
	}
      return( TRUE );
    }
  return( FALSE );
} 

static BOOL InitEdgeTable( int nSize )
{
  register int nIndex;
  
  /*     Initialize the edge table */
  PolEdges = (ListHead**) malloc(sizeof(ListHead*) * nSize );
  if ( PolEdges )
    {
      for ( nIndex=0; nIndex < nSize; nIndex++ )
	{
	  PolEdges[nIndex] = NULL;
	}
      return( TRUE );
    }
  return( FALSE );
}


void InitStripTable(  )
{
  
  PLISTHEAD pListHead;
  
  /*   Initialize the strip table */
  pListHead = ( PLISTHEAD ) malloc(sizeof(ListHead));
  if ( pListHead )
    {
      InitList( pListHead );
      strips[0] = pListHead;
    }
  else
    {
      printf("Out of memory !\n");
      exit(0);
    }
  
}

void Init_Table_SGI(int numfaces)
{
  PLISTHEAD pListHead;
  int max_adj = 60;
  register int x;
  
  /*   This routine will initialize the table that will
       have the faces sorted by the number of adjacent polygons
       to it.
  */
  
  for (x=0; x< max_adj; x++)
    {
      /*   We are allowing the max number of sides of a polygon
	   to be max_adj.
      */                      
      pListHead = ( PLISTHEAD ) malloc(sizeof(ListHead));
      if ( pListHead )
	{
	  InitList( pListHead );
	  array[x] = pListHead;
	}
      else
	{
	  printf("Out of memory !\n");
	  exit(0);
	}
    }
  
  if (face_array != NULL) /* It seems this function is called more than */
    free(face_array);     /* once so we'll free up the old stuff */
  
  face_array = (P_FACE_ADJACENCIES) malloc (sizeof(FACE_ADJACENCIES) * numfaces); 
  if (face_array == NULL) {
    printf("Out of memory !!\n");
    exit(0);
  }
  
}



static void BuildVertexTable( int nSize )
{
  register int nIndex;
  PLISTHEAD pListHead;
  
  for ( nIndex=0; nIndex < nSize; nIndex++ )
    {
      pListHead = ( PLISTHEAD ) malloc(sizeof(ListHead));
      if ( pListHead )
	{
	  InitList( pListHead );
	  Vertices[nIndex] = pListHead;
	}
      else
	return; 
      
    }
}

static void BuildFaceTable( int nSize )
{
  register int nIndex;
  PLISTHEAD pListHead;
  
  for ( nIndex=0; nIndex < nSize; nIndex++ )
    {
      pListHead = ( PLISTHEAD ) malloc(sizeof(ListHead));
      if ( pListHead )
	{
	  InitList( pListHead );
	  PolFaces[nIndex] = pListHead;
	}
      else
	return; 
      
    }
}

static void BuildEdgeTable( int nSize )
{
  register int nIndex;
  PLISTHEAD pListHead;
  
  for ( nIndex=0; nIndex < nSize; nIndex++ )
    {
      pListHead = ( PLISTHEAD ) malloc(sizeof(ListHead));
      if ( pListHead )
	{
	  InitList( pListHead );
	  PolEdges[nIndex] = pListHead;
	}
      else
	return;
    }
}

void Start_Vertex_Struct(int numverts)
{
  if (InitVertexTable(numverts))
    {
      BuildVertexTable(numverts);
    }
}

void Start_Face_Struct(int numfaces)
{
  if (InitFaceTable(numfaces))
    {
      BuildFaceTable(numfaces);
    }
}

void Start_Edge_Struct(int numverts)
{
  if (InitEdgeTable(numverts))
    {
      BuildEdgeTable(numverts);
    }
}
