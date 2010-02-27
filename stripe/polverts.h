/********************************************************************/
/*   STRIPE: converting a polygonal model to triangle strips    
     Francine Evans, 1996.
     SUNY @ Stony Brook
     Advisors: Steven Skiena and Amitabh Varshney

     Modified by Elvir Azanli, 1998
*/
/********************************************************************/

/*---------------------------------------------------------------------*/
/*   STRIPE: polverts.h
-----------------------------------------------------------------------*/

#ifndef POLVERTS_INCLUDED
#define POLVERTS_INCLUDED

#include "queue.h"
	
typedef struct adjacencies
{
	Node ListNode;
	int face_id;
} ADJACENCIES,*P_ADJACENCIES;

typedef struct FVerts
{
	Node ListNode;
	int *pPolygon;
	int nPolSize;
	int nId;
} F_VERTS, *PF_VERTS;

/*Every time we need to use this, cast it ( ListInfo*)*/

typedef struct FEdges
{
	Node ListNode;
	int edge[3];
}F_EDGES,*PF_EDGES;

typedef struct FFaces
{
	Node ListNode;
	int *pPolygon;
	int *pNorms;
    int     seen;
    int seen2;
    int seen3;
	int nPolSize;
  int nOrgSize;
	F_EDGES **VertandId;
	int *marked;
		int *walked;
} F_FACES,*PF_FACES;
	
typedef struct FVertices 
  { 
     Node ListNode;
     PF_FACES face;
  } F_VERTICES, *PF_VERTICES;

typedef struct Strips
{
	Node ListNode;
	int face_id;
} Strips,*P_STRIPS;


      struct vert_added
     {
          int num;
          int *normal;
     };


typedef struct face_adjacencies {
    P_ADJACENCIES pfNode;
    int bucket;
    ListHead *head;
  } FACE_ADJACENCIES, *P_FACE_ADJACENCIES;

/*      Globals */
int num_faces;
ListHead **PolFaces;
ListHead **PolEdges;
ListHead *array[60];
P_FACE_ADJACENCIES face_array;  /* Pointers from face_id to face   */
ListHead **Vertices;            /* Pointers from vertex_id to face */
ListHead *strips[1];
int orient;
#endif
