#include "StdAfx.h"

extern "C" {

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/bigarray.h>

CAMLprim value
run_strips (value v_topology)
{
  CAMLparam1 (v_topology);
  long num_faces, inp_length = Bigarray_val (v_topology)->dim[0];
  udword *topology = (udword *) Data_bigarray_val (v_topology);
  CAMLlocal3 (result_strip, result_strip_list, cons);
  
  if ((inp_length % 3) != 0)
    fprintf (stderr, "Warning: input points not a multiple of 3.\n");
  
  num_faces = inp_length / 3;
  
  if (num_faces == 0)
    CAMLreturn (Val_emptylist);
  
#if 0
  fprintf (stderr, "Number of faces: %d\n", (int) num_faces);
  for (int i = 0; i < num_faces; i++)
    {
      fprintf (stderr, "Face %d: (%d, %d, %d)\n", i, topology[i * 3],
	       topology[i * 3 + 1], topology[i * 3 + 2]);
    }
#endif
  
  STRIPERCREATE sc;
  sc.DFaces = topology;
  sc.NbFaces = num_faces;
  sc.AskForWords = false;
  sc.ConnectAllStrips = false;
  sc.OneSided = true;
  sc.SGIAlgorithm = false;
  
  Striper Strip;
  if (!Strip.Init (sc))
    {
      fprintf (stderr, "Strip init failed!\n");
      exit (1);
    }
  
  STRIPERRESULT sr;
  if (!Strip.Compute (sr))
    {
      fprintf (stderr, "Uh-oh, strips returned false.\n");
      exit (1);
    }
  
  result_strip_list = Val_emptylist;

  udword* Refs = (udword*)sr.StripRuns;
  for (udword i = 0; i < sr.NbStrips; i++)
    {
      udword NbRefs = sr.StripLengths[i];
      
      result_strip = Val_emptylist;
      
      for (udword j = 0; j < NbRefs; j++)
        {
	  cons = caml_alloc (2, Tag_cons);
	  
	  Store_field (cons, 0, Val_int (*Refs++));
	  Store_field (cons, 1, result_strip);
	  
	  result_strip = cons;
	}

      cons = caml_alloc (2, Tag_cons);
      
      Store_field (cons, 0, result_strip);
      Store_field (cons, 1, result_strip_list);
      
      result_strip_list = cons;
    }

  CAMLreturn (result_strip_list);
}

}
