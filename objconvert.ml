(* Nasty program to scrape data out of Collada "dae"-like files.  *)

open Genlex

exception Unknown_node of string

let (float_arrays : (string, float array) Hashtbl.t) = Hashtbl.create 5

type tag = [ `misc of string | `u | `v | `s | `t | `x | `y | `z ]

type accessor = int * int * (int -> tag -> float)

let (accessors : (string, accessor) Hashtbl.t) = Hashtbl.create 5

type source =
  {
    mutable data_array : string;
    mutable accessor : string
  }

let (sources : (string, source) Hashtbl.t) = Hashtbl.create 5

let do_later = ref []

type vertex =
  {
    mutable x : float;
    mutable y : float;
    mutable z : float
  }

let (vertices : (string, vertex array) Hashtbl.t) = Hashtbl.create 5

type poly_type =
    Polygons
  | Triangles
  | Polylist

type polygon =
  {
    mutable geometry : string;
    mutable vertex : (string * int) option;
    mutable normal : (string * int) option;
    mutable texcoord : (string * int) option;
    mutable uv : (string * int) option;
    mutable polys : int array list;
    mutable poly_type : poly_type;
    mutable vcount : int list;
    mutable material : string option
  }

type surface_type = Surftype_2D

and surface_format = A8R8G8B8

and material =
  {
    mutable image : string;
    mutable surface_type : surface_type;
    mutable surface_format : surface_format;
    mutable phong : phong;
  }

and phong =
  {
    mutable emission : colour option;
    mutable ambient : colour option;
    mutable diffuse : covering option;
    mutable specular : colour option;
    mutable shininess : float option;
    mutable reflective : colour option;
    mutable reflectivity : float option;
    mutable transparent : colour option;
    mutable transparency : float option
  }

and lambert =
  {
    mutable l_ambient : colour option;
    mutable l_diffuse : covering option
  }

and colour =
  {
    r : float;
    g : float;
    b : float;
    a : float
  }

and texture =
  {
    t_texcoord : string;
    t_texture : string
  }

and covering = Texture of texture | Colour of colour

let geometries = ref []

let (images : (string, string) Hashtbl.t) = Hashtbl.create 5

let bad_node node complaint =
  let entity, line, pos = node#position in
  Printf.fprintf stderr "At %s, line %d, pos %d:\n" entity line pos;
  raise (Unknown_node complaint)

let resolve_local_ref src =
  if src.[0] = '#' then
    String.sub src 1 ((String.length src) - 1)
  else
    failwith (src ^ "not a local reference")

let string_data_for_node node =
  let data = ref None in
  node#iter_nodes_sibl
    (fun pred_node child succ_node ->
      if pred_node <> None && succ_node <> None then
        raise (Unknown_node "Expected a single string datum");
      match child#node_type with
        Pxp_document.T_data -> data := Some child#data
      | _ -> raise (Unknown_node "Not a data node"));
  match !data with
    Some d -> d
  | None -> raise Not_found

let node_data_count node =
  let num = ref 0 in
  node#iter_nodes
    (fun child ->
      match child#node_type with
        Pxp_document.T_data -> incr num
      | _ -> ());
  !num

let has_single_datum node =
  node_data_count node = 1

let node_child node =
  match node#sub_nodes with
    [sub] -> sub
  | _ -> bad_node node "wrong number of children"

type asset =
  {
    mutable authoring_tool : string option;
    mutable created : string option;
    mutable modified : string option;
    mutable revision : string option;
    mutable units : string option;
    mutable up_axis : string option
  }

let get_assets root_node =
  let my_asset = { authoring_tool = None; created = None; modified = None;
		   revision = None; units = None; up_axis = None } in
  let asset_node = Pxp_document.find
    (fun node ->
      match node#node_type with
        Pxp_document.T_element "asset" -> true
      | _ -> false)
    root_node in
  asset_node#iter_nodes
    (fun node ->
      match node#node_type with
        Pxp_document.T_element "contributor" ->
	  node#iter_nodes
	    (fun con_node ->
	      match con_node#node_type with
		Pxp_document.T_element "authoring_tool" ->
	          my_asset.authoring_tool
		    <- Some con_node#data
	      | _ -> ())
      | Pxp_document.T_element "created" ->
          my_asset.created <- Some node#data
      | Pxp_document.T_element "modified" ->
          my_asset.modified <- Some node#data
      | Pxp_document.T_element "revision" ->
          my_asset.revision <- Some node#data
      | Pxp_document.T_element "unit" ->
          my_asset.units <- Some (node#required_string_attribute "meter")
      | Pxp_document.T_element "up_axis" ->
	  my_asset.up_axis <- Some node#data
      | _ -> raise (Unknown_node "below asset"));
  my_asset
     
let print_assets assets =
  let opt = function
    Some n -> n
  | None -> "(none)" in
  Printf.printf "Authoring tool: %s\n" (opt assets.authoring_tool);
  Printf.printf "Created: %s\n" (opt assets.created);
  Printf.printf "Modified: %s\n" (opt assets.modified);
  Printf.printf "Revision: %s\n" (opt assets.revision);
  Printf.printf "Units: %sm\n" (opt assets.units);
  Printf.printf "Up axis: %s\n" (opt assets.up_axis)

(* Find all nodes with tag TAG, which are the immediate children of
   ROOT_NODE.  *)

let find_all_by_tag root_node tag =
  Pxp_document.find_all
    (fun node ->
      match node#node_type with
	Pxp_document.T_element t when t = tag -> true
      | _ -> false)
    root_node

type params = param_type * string

and param_type =
    Float_param
  | Int_param
  | Boolean_param

let parse_params param_nodes =
  let (param_list : params list ref) = ref [] in
  List.iter
    (fun param ->
      match param#node_type with
        Pxp_document.T_element "param" ->
	  let name = param#required_string_attribute "name"
	  and ptype = param#required_string_attribute "type" in
	  let add_param =
	    if has_single_datum param then
	      failwith "Params with data not implemented"
	    else begin
	      match ptype with
		"float" -> Float_param, name
	      | "int" -> Int_param, name
	      | "bool" -> Boolean_param, name
	      | x -> failwith ("Unknown param type " ^ x)
	    end in
	  param_list := add_param :: !param_list
      | _ -> raise (Unknown_node "below param"))
    param_nodes;
  List.rev !param_list

let make_accessor source_id array_source count stride param_list =
  let rec scan_params acc plist pnum =
    match plist with
      [] -> acc
    | (ptype, pname)::ps ->
        if ptype <> Float_param then
	  failwith "Non-float accessor not implemented";
        let tag = begin match pname with
	  "X" -> `x | "Y" -> `y | "Z" -> `z | "S" -> `s | "T" -> `t
	| "U" -> `u | "V" -> `v | other -> `misc other
	end in
	scan_params ((tag, pnum) :: acc) ps (succ pnum) in
  let param_map = scan_params [] param_list 0 in
  let accessor idx tag =
    let values = Hashtbl.find float_arrays array_source in
    let component = List.assoc tag param_map in
    values.(idx * stride + component) in
  Hashtbl.add accessors source_id (count, stride, accessor)

let parse_technique_common source_id tc_nodes =
  List.iter
    (fun tc_part ->
      match tc_part#node_type with
        Pxp_document.T_element "accessor" ->
	  let array_source
	    = resolve_local_ref (tc_part#required_string_attribute "source")
	  and count = tc_part#required_string_attribute "count"
	  and stride = tc_part#required_string_attribute "stride" in
	  let icount = int_of_string count
	  and istride = int_of_string stride in
	  let param_list = parse_params tc_part#sub_nodes in
	  make_accessor source_id array_source icount istride param_list
      | _ -> raise (Unknown_node "below technique_common"))
    tc_nodes

exception Bad_token of Genlex.token

let parse_float_array count datastring =
  let myarray = Array.make count 0.0 in
  let lexer = Genlex.make_lexer [] (Stream.of_string datastring) in
  for i = 0 to (count - 1) do
    let item = Stream.next lexer in
    match item with
      Genlex.Float f -> myarray.(i) <- f
    | Genlex.Int n -> myarray.(i) <- float_of_int n
    | _ -> raise (Bad_token item)
  done;
  myarray

let parse_int_list datastring =
  let lexer = Genlex.make_lexer [] (Stream.of_string datastring) in
  let rec accumulate entries =
    try
      let item = Stream.next lexer in
      match item with
	Genlex.Int n -> accumulate (n :: entries)
      | _ -> raise (Bad_token item)
    with Stream.Failure -> entries in
  List.rev (accumulate [])

let parse_colour node =
  let r = ref 0.0 and g = ref 0.0 and b = ref 0.0 and a = ref 0.0 in
  begin match node#node_type with
    Pxp_document.T_element "color" ->
      let values = parse_float_array 4 node#data in
      r := values.(0);
      g := values.(1);
      b := values.(2);
      a := values.(3)
  | _ -> bad_node node "not color"
  end;
  { r = !r; g = !g; b = !b; a = !a }

let parse_float node =
  let myfloat = ref 0.0 in
  begin match node#node_type with
    Pxp_document.T_element "float" ->
      let v = parse_float_array 1 node#data in
      myfloat := v.(0)
  | _ -> bad_node node "not float"
  end;
  !myfloat

let parse_covering covering =
  match covering#node_type with
    Pxp_document.T_element "texture" ->
      let texcoord
	= covering#required_string_attribute "texcoord"
      and texture
	= covering#required_string_attribute "texture" in
      Texture { t_texcoord = texcoord; t_texture = texture }
  | Pxp_document.T_element "color" ->
      let col = parse_colour covering in
      Colour col
  | _ -> bad_node covering "covering"

let parse_phong id phong_parts =
  let phonginf =
    {
      emission = None;
      ambient = None;
      diffuse = None;
      specular = None;
      shininess = None;
      reflective = None;
      reflectivity = None;
      transparent = None;
      transparency = None;
    } in
  List.iter
    (fun node ->
      match node#node_type with
        Pxp_document.T_element "ambient" ->
	  phonginf.ambient <- Some (parse_colour (node_child node))
      | Pxp_document.T_element "diffuse" ->
          phonginf.diffuse <- Some (parse_covering (node_child node))
      | Pxp_document.T_element "specular" ->
	  phonginf.specular <- Some (parse_colour (node_child node))
      | Pxp_document.T_element "shininess" ->
	  phonginf.shininess <- Some (parse_float (node_child node))
      | Pxp_document.T_element "emission" ->
	  phonginf.emission <- Some (parse_colour (node_child node))
      | Pxp_document.T_element "reflective" ->
	  phonginf.reflective <- Some (parse_colour (node_child node))
      | Pxp_document.T_element "reflectivity" ->
	  phonginf.reflectivity <- Some (parse_float (node_child node))
      | Pxp_document.T_element "transparent" ->
	  phonginf.transparent <- Some (parse_colour (node_child node))
      | Pxp_document.T_element "transparency" ->
	  phonginf.transparency <- Some (parse_float (node_child node))
      | _ -> bad_node node "below phong")
    phong_parts

let parse_lambert id lambert_parts =
  let lambertinf =
    {
      l_ambient = None;
      l_diffuse = None
    } in
  List.iter
    (fun node ->
      match node#node_type with
        Pxp_document.T_element "ambient" ->
	  lambertinf.l_ambient <- Some (parse_colour (node_child node))
      | Pxp_document.T_element "diffuse" ->
          lambertinf.l_diffuse <- Some (parse_covering (node_child node))
      | _ -> bad_node node "below lambert")
    lambert_parts

let parse_technique id sid nodes =
  List.iter
    (fun node ->
      match node#node_type with
        Pxp_document.T_element "phong" ->
	  parse_phong id node#sub_nodes
      | Pxp_document.T_element "lambert" ->
          parse_lambert id node#sub_nodes
      | Pxp_document.T_element "program" ->
          (* Ignore. *)
          ()
      | _ -> bad_node node "below technique")
    nodes

let parse_source source_id source_nodes =
  List.iter
    (fun source_part ->
      match source_part#node_type with
        Pxp_document.T_element "float_array" ->
	  let id = source_part#required_string_attribute "id"
	  and count = source_part#required_string_attribute "count" in
	  let icount = int_of_string count in
	  let f_arr = parse_float_array icount source_part#data in
	  Hashtbl.add float_arrays id f_arr
      | Pxp_document.T_element "technique_common" ->
          parse_technique_common source_id source_part#sub_nodes
      | Pxp_document.T_element "technique" ->
          let id = source_part#optional_string_attribute "id"
	  and sid = source_part#optional_string_attribute "sid" in
          parse_technique id sid source_part#sub_nodes
      | Pxp_document.T_element x -> raise (Unknown_node (x ^ " below source"))
      | Pxp_document.T_data ->
          let mydata = source_part#data in
	  Printf.printf "Data element: id=%s data=%s\n" source_id mydata
      | _ -> raise (Unknown_node "below source"))
    source_nodes

type semantic =
    Position
  | Vertex
  | Normal
  | Texcoord
  | UV

let parse_input input_node =
  let source = resolve_local_ref (input_node#required_string_attribute "source")
  and offset = input_node#attribute "offset" in
  let sem = match input_node#required_string_attribute "semantic" with
    "POSITION" -> Position
  | "VERTEX" -> Vertex
  | "NORMAL" -> Normal
  | "TEXCOORD" -> Texcoord
  | "UV" -> UV
  | _ -> failwith "Unknown semantic" in
  sem, source, offset

(* I'm sure this is totally bogus in the general case...  *)

let parse_vertices id vert_nodes =
  List.iter
    (fun vert_part ->
      match vert_part#node_type with
        Pxp_document.T_element "input" ->
	  let sem, source, offset = parse_input vert_part in
	  if offset <> Pxp_types.Implied_value then
	    failwith "Didn't expect an offset for vertices";
	  let later () =
	    let count, stride, accessor = Hashtbl.find accessors source in
	    (* Careful here, Array.make sets all elements to be physically
	       equal, in the sense of "==" returning true.  That doesn't play
	       well with mutable values.  *)
	    let v_array = Array.init count
	                    (fun _ -> { x = 0.0; y = 0.0; z = 0.0 }) in
	    assert (stride == 3);
	    begin match sem with
	      Position ->
		for i = 0 to (count - 1) do
		  v_array.(i).x <- accessor i `x;
		  v_array.(i).y <- accessor i `y;
		  v_array.(i).z <- accessor i `z
		done
	    | _ -> raise (Unknown_node "vertices input semantic")
	    end;
	    Hashtbl.add vertices id v_array
	    in
	  do_later := later :: !do_later
      | _ -> raise (Unknown_node "below vertices"))
    vert_nodes

let parse_polygons geom_id count poly_nodes ~poly_type ~material =
  let vertex = ref None
  and normal = ref None
  and texcoord = ref None
  and uv = ref None
  and polys = ref []
  and vcount = ref [] in
  List.iter
    (fun poly_part ->
      match poly_part#node_type with
        Pxp_document.T_element "input" ->
	  let sem, source, offset = parse_input poly_part in
	  let ioffset = match offset with
	    Pxp_types.Implied_value -> failwith "Expected offset"
	  | Pxp_types.Value v -> int_of_string v
	  | _ -> failwith "Unexpected value" in
	  begin match sem with
	    Vertex -> vertex := Some (source, ioffset)
	  | Normal -> normal := Some (source, ioffset)
	  | Texcoord -> texcoord := Some (source, ioffset)
	  | UV -> uv := Some (source, ioffset)
	  | _ -> failwith "Unexpected source in polygons"
	  end
      | Pxp_document.T_element "p" ->
          let data = poly_part#data in
	  let idx_list = parse_int_list data in
	  polys := (Array.of_list idx_list) :: !polys
      | Pxp_document.T_element "extra" -> ()
      | Pxp_document.T_element "vcount" ->
	  let data = poly_part#data in
	  let idx_list = parse_int_list data in
	  vcount := idx_list
      | _ -> raise (Unknown_node "below polygons"))
    poly_nodes;
  let poly =
    {
      geometry = geom_id;
      vertex = !vertex;
      normal = !normal;
      texcoord = !texcoord;
      uv = !uv;
      polys = !polys;
      poly_type = poly_type;
      vcount = !vcount;
      material = material
    } in
  geometries := poly :: !geometries

let parse_mesh geom_id mesh_nodes =
  List.iter
    (fun mesh_part ->
      match mesh_part#node_type with
        Pxp_document.T_element "source" ->
	  let id = mesh_part#required_string_attribute "id" in
	  parse_source id mesh_part#sub_nodes;
      | Pxp_document.T_element "vertices" ->
          let id = mesh_part#required_string_attribute "id" in
          parse_vertices id mesh_part#sub_nodes
      | Pxp_document.T_element "polygons" ->
          let count = mesh_part#required_string_attribute "count"
	  and material = mesh_part#optional_string_attribute "material" in
	  let icount = int_of_string count in
          parse_polygons geom_id icount mesh_part#sub_nodes ~poly_type:Polygons
			 ~material:material
      | Pxp_document.T_element "triangles" ->
	  let count = mesh_part#required_string_attribute "count"
	  and material = mesh_part#optional_string_attribute "material" in
	  let icount = int_of_string count in
	  parse_polygons geom_id icount mesh_part#sub_nodes ~poly_type:Triangles
			 ~material:material
      | Pxp_document.T_element "polylist" ->
	  let count = mesh_part#required_string_attribute "count"
	  and material = mesh_part#optional_string_attribute "material" in
	  let icount = int_of_string count in
	  parse_polygons geom_id icount mesh_part#sub_nodes ~poly_type:Polylist
			 ~material:material
      | Pxp_document.T_element x -> raise (Unknown_node (x ^ " below mesh"))
      | _ -> raise (Unknown_node "below mesh"))
    mesh_nodes

let parse_geometry id name geom_nodes =
  List.iter
    (fun geom_part ->
      match geom_part#node_type with
        Pxp_document.T_element "mesh" ->
	  parse_mesh id geom_part#sub_nodes
      | Pxp_document.T_element "spline" ->
          print_endline "Spline ignored in geometry"
      | Pxp_document.T_element "extra" -> ()
      | _ -> raise (Unknown_node "below geometry"))
    geom_nodes

let get_library_geometries root_node =
  let lib_geoms = find_all_by_tag root_node "library_geometries" in
  List.iter
    (fun lib_geom ->
      lib_geom#iter_nodes
        (fun node ->
	  match node#node_type with
	    Pxp_document.T_element "geometry" ->
	      let id = node#required_string_attribute "id"
	      and name = node#optional_string_attribute "name" in
	      parse_geometry id name node#sub_nodes
	  | _ -> raise (Unknown_node "below library_geometries")))
    lib_geoms

let parse_image id image_nodes =
  List.iter
    (fun image_part ->
      match image_part#node_type with
        Pxp_document.T_element "init_from" ->
	  Hashtbl.add images id image_part#data
	| _ -> raise (Unknown_node "below image"))
    image_nodes

let get_library_images root_node =
  let lib_images = find_all_by_tag root_node "library_images" in
  List.iter
    (fun lib_image ->
      lib_image#iter_nodes
        (fun node ->
	  match node#node_type with
	    Pxp_document.T_element "image" ->
	      let id = node#required_string_attribute "id" in
	      parse_image id node#sub_nodes
	  | _ -> raise (Unknown_node "below library_images")))
    lib_images

let parse_newparam sid newparam_nodes =
  List.iter
    (fun newparam_part ->
      match newparam_part#node_type with
        Pxp_document.T_element "surface" ->
	  let stype = newparam_part#required_string_attribute "type" in
	  (* init_from, format *)
	  ignore stype
      | Pxp_document.T_element "sampler2D" ->
          (* minfilter, magfilter *)
          ()
      | _ -> raise (Unknown_node "below newparam"))
    newparam_nodes

let parse_profile_common id profile_common_parts =
  List.iter
    (fun prof_com_part ->
      match prof_com_part#node_type with
        Pxp_document.T_element "technique" ->
	  let id = prof_com_part#optional_string_attribute "id"
	  and sid = prof_com_part#optional_string_attribute "sid" in
	  parse_technique id sid prof_com_part#sub_nodes
      | Pxp_document.T_element "newparam" ->
          let sid = prof_com_part#optional_string_attribute "sid" in
	  parse_newparam sid prof_com_part#sub_nodes
      | _ -> bad_node prof_com_part "below profile_common")
    profile_common_parts

let parse_effect id effect_nodes =
  List.iter
    (fun effect_part ->
      match effect_part#node_type with
        Pxp_document.T_element "profile_COMMON" ->
	  parse_profile_common id effect_part#sub_nodes
      | _ -> bad_node effect_part "below effect")
    effect_nodes

let get_library_effects root_node =
  let lib_effects = find_all_by_tag root_node "library_effects" in
  List.iter
    (fun lib_effect ->
      lib_effect#iter_nodes
        (fun node ->
	  match node#node_type with
	    Pxp_document.T_element "effect" ->
	      let id = node#required_string_attribute "id" in
	      parse_effect id node#sub_nodes
	  | _ -> raise (Unknown_node "below library_effects")))
    lib_effects

let print_type = function
    Pxp_document.T_element t -> Printf.sprintf "<%s>" t
  | Pxp_document.T_data -> "data"
  | Pxp_document.T_comment -> "comment"
  | Pxp_document.T_pinstr n -> Printf.sprintf "pinstr (%s)" n
  | Pxp_document.T_super_root -> "super_root"
  | Pxp_document.T_attribute n -> Printf.sprintf "attribute (%s)" n
  | Pxp_document.T_namespace p -> Printf.sprintf "namespace (%s)" p
  | Pxp_document.T_none -> "none"

let rec scan_doc node ind =
  Printf.printf "%s" (String.make ind ' ');
  Printf.printf "Node type: %s\n" (print_type node#node_type);
  begin match node#node_type with
    Pxp_document.T_data ->
      let mydata = node#data in
      Printf.printf "data is '%s'\n" mydata
  | _ -> ()
  end;
  let sub_nodes = node#sub_nodes in
  List.iter (fun node -> scan_doc node (ind + 2)) sub_nodes

let print_vertices vertices =
  Hashtbl.iter
    (fun name v_array ->
      Printf.printf "Vertex array %s:\n" name;
      for i = 0 to (Array.length v_array) - 1 do
        Printf.printf "  %d: %f %f %f\n" i v_array.(i).x v_array.(i).y
		      v_array.(i).z
      done)
    vertices

let vertex_offset poly =
  match poly.vertex with
    None -> raise Not_found
  | Some (_, o) -> o

let normal_offset poly =
  match poly.normal with
    None -> raise Not_found
  | Some (_, o) -> o

let texcoord_offset poly =
  match poly.texcoord with
    None -> raise Not_found
  | Some (_, o) -> o

let uv_offset poly =
  match poly.uv with
    None -> raise Not_found
  | Some (_, o) -> o

let poly_stride poly =
  let highest = ref 0 in
  let increase_to fn =
    try
      let h = fn poly in
      if h > !highest then
        highest := h
    with Not_found -> () in
  increase_to vertex_offset;
  increase_to normal_offset;
  increase_to texcoord_offset;
  increase_to uv_offset;
  succ !highest

let print_geometries geometries =
  List.iter
    (fun poly ->
      let stride = poly_stride poly in
      Printf.printf "For geometry %s:\n" poly.geometry;
      Printf.printf "Material is %s\n"
        (match poly.material with None -> "(not set)" | Some x -> x);
      List.iter
        (fun idx_array ->
	  let points = (Array.length idx_array) / stride in
	  begin match poly.vertex with
	    Some (name, offset) ->
	      let v_array = Hashtbl.find vertices name in
	      Printf.printf "%d vertices:\n" points;
	      for idx = 0 to points - 1 do
	        let pos = idx_array.(idx * stride + offset) in
		Printf.printf "%d: (%f %f %f)\n" pos
		  v_array.(pos).x v_array.(pos).y v_array.(pos).z
	      done
	  | None -> print_endline "No vertices found"
	  end;
	  begin match poly.normal with
	    Some (name, offset) ->
	      let _, _, norm_accessor = Hashtbl.find accessors name in
	      print_endline "Normals:";
	      for idx = 0 to points - 1 do
	        let pos = idx_array.(idx * stride + offset) in
		Printf.printf "%d: (%f %f %f)\n" pos
		  (norm_accessor pos `x) (norm_accessor pos `y)
		  (norm_accessor pos `z)
	      done
	  | None -> print_endline "No normals found"
	  end;
	  begin match poly.texcoord with
	    Some (name, offset) ->
	      let _, _, texcoord_accessor = Hashtbl.find accessors name in
	      print_endline "Texcoord (s,t):";
	      for idx = 0 to points - 1 do
	        let pos = idx_array.(idx * stride + offset) in
		Printf.printf "%d: (%f %f)\n" pos (texcoord_accessor pos `s)
		  (texcoord_accessor pos `t)
	      done
	  | None -> print_endline "No texcoords found"
	  end;
	  begin match poly.uv with
	    Some (name, offset) ->
	      let _, _, uv_accessor = Hashtbl.find accessors name in
	      print_endline "UVs:";
	      for idx = 0 to points - 1 do
	        let pos = idx_array.(idx * stride + offset) in
		Printf.printf "%d: (%f %f)\n" pos (uv_accessor pos `u)
		  (uv_accessor pos `v)
	      done
	  | None -> print_endline "No UVs found"
	  end)
	poly.polys)
    geometries

let geometry_list geometries =
  List.fold_right
    (fun poly geoms ->
      if not (List.mem poly.geometry geoms) then
	poly.geometry :: geoms
      else
	geoms)
    geometries
    []

let vertices_for_geometry geometries which =
  List.fold_right
    (fun poly vert_src ->
      if poly.geometry = which then begin
        match poly.vertex with
	  Some (name, offset) ->
	    if not (List.mem name vert_src) then
	      name :: vert_src
	    else
	      vert_src
	| None -> vert_src
      end else
        vert_src)
    geometries
    []

let normals_for_geometry geometries which =
  List.fold_right
    (fun poly vert_src ->
      if poly.geometry = which then begin
        match poly.normal with
	  Some (name, offset) ->
	    if not (List.mem name vert_src) then
	      name :: vert_src
	    else
	      vert_src
	| None -> vert_src
      end else
        vert_src)
    geometries
    []

let emit_obj fo geometries geom =
  let sources = vertices_for_geometry geometries geom in
  List.iter
    (fun source ->
      Printf.fprintf fo "# geometry %s, source %s\n" geom source;
      let v_array = Hashtbl.find vertices source in
      for i = 0 to Array.length v_array - 1 do
	Printf.fprintf fo "v %f %f %f\n" v_array.(i).x v_array.(i).y
	  v_array.(i).z
      done)
    sources;
  let nsources = normals_for_geometry geometries geom in
  List.iter
    (fun source ->
      Printf.fprintf fo "# geometry %s, source %s\n" geom source;
      let _, _, norm_accessor = Hashtbl.find accessors source in
      let i = ref 0 in
      try
	while true do
	  Printf.fprintf fo "vn %f %f %f\n" (norm_accessor !i `x)
	    (norm_accessor !i `y) (norm_accessor !i `z);
	  incr i
	done
      with Invalid_argument _ ->
	())
    nsources;
  List.iter
    (fun poly ->
      if poly.geometry = geom then begin
	let stride = poly_stride poly in
	List.iter
	  (fun idx_array ->
	    let points = (Array.length idx_array) / stride in
	    Printf.fprintf fo "f";
	    for idx = 0 to points - 1 do
	      let vtx = match poly.vertex with
		Some (_, offset) ->
		  string_of_int (1 + idx_array.(idx * stride + offset))
	      | None -> "" in
	      let nrm = match poly.normal with
		Some (_, offset) ->
		  string_of_int (1 + idx_array.(idx * stride + offset))
	      | None -> "" in
	      Printf.fprintf fo " %s//%s" vtx nrm
	    done;
	    Printf.fprintf fo "\n")
	  poly.polys
      end)
    geometries

let objf_lexer = Genlex.make_lexer ["v"; "vn"; "t"; "q"; "//"; "/"]

let all_strips = ref []
let accum = ref []
let verts = ref []
let norms = ref []

let rec objf_parse_line = parser
    [< 'Kwd "v"; 'Float vx; 'Float vy; 'Float vz >] ->
      verts := (vx, vy, vz) :: !verts
  | [< 'Kwd "vn"; 'Float nx; 'Float ny; 'Float nz >] ->
      norms := (nx, ny, nz) :: !norms
  | [< 'Kwd "t"; args = list_of_ints >] ->
      if !accum <> [] then
        all_strips := !accum :: !all_strips;
      accum := args
  | [< 'Kwd "q"; args = list_of_ints >] ->
      accum := !accum @ args
and list_of_ints = parser
    [< 'Int i; 'Kwd "//"; 'Int n; rest = list_of_ints >] ->
      (i, n) :: rest
  | [< >] -> []

let reload_objf fi =
  try
    while true do
      let line = input_line fi in
      if String.length line > 0 && line.[0] <> '#' then
	objf_parse_line (objf_lexer (Stream.of_string line))
    done;
    raise End_of_file
  with End_of_file ->
    if !accum <> [] then
      all_strips := !accum :: !all_strips;
    !all_strips

(* Output format:
     strip <int>
     verts
     <float> <float> <float>
     <float> <float> <float>
     ...
     norms
     <float> <float> <float>
     ...
     texcoords
     <float> <float>
     ...
     end
*)

let strip_geometries geometries =
  let glist = geometry_list geometries in
  List.iter
    (fun geom ->
      all_strips := [];
      accum := [];
      verts := [];
      norms := [];
      let fo = open_out "test.obj" in
      emit_obj fo geometries geom;
      close_out fo;
      Sys.remove "test.objf";
      ignore (Sys.command "stripe/stripe -w test.obj");
      let fi = open_in "test.objf" in
      let strips = reload_objf fi in
      ignore strips;
      close_in fi;
      let v_arr = Array.of_list (List.rev !verts) in
      let n_arr = Array.of_list (List.rev !norms) in
      let outf = open_out "out.stp" in
      List.iter
        (fun strip ->
	  Printf.fprintf outf "strip %d\n" (List.length strip);
	  Printf.fprintf outf "verts\n";
	  List.iter
	    (fun (v, _) ->
	      let (x, y, z) = v_arr.(v - 1) in
	      Printf.fprintf outf "%f %f %f\n" x y z)
	    strip;
	  Printf.fprintf outf "norms\n";
	  List.iter
	    (fun (_, n) ->
	      let (x, y, z) = n_arr.(n - 1) in
	      Printf.fprintf outf "%f %f %f\n" x y z)
	    strip;
	  Printf.fprintf outf "end\n")
	!all_strips;
      close_out outf)
    glist

let add_if_different vx vy vz nx ny nz counted =
  let epsilon = 0.000001 in
  let rec scan idx = function
    (vx', vy', vz', nx', ny', nz') :: more ->
      if abs_float (vx' -. vx) <= epsilon
         && abs_float (vy' -. vy) <= epsilon
	 && abs_float (vz' -. vz) <= epsilon
	 && abs_float (nx' -. nx) <= epsilon
	 && abs_float (ny' -. ny) <= epsilon
	 && abs_float (nz' -. nz) <= epsilon then
	idx
      else
        scan (idx - 1) more
  | _ -> raise Not_found in
  try
    let idx = scan (List.length counted - 1) counted in
    idx, counted
  with Not_found ->
    let idx = List.length counted in
    idx, (vx, vy, vz, nx, ny, nz) :: counted

let add_polygons poly idx_array stride triangle_indices counted_pts =
  let counted = ref counted_pts in
  let points = (Array.length idx_array) / stride in
  match poly.vertex, poly.normal with
    Some (vname, voffset), Some (nname, noffset) ->
      let v_array = Hashtbl.find vertices vname in
      let _, _, norm_accessor = Hashtbl.find accessors nname in
      let face = Array.create points (-1) in
      for idx = 0 to points - 1 do
	let vpos = idx_array.(idx * stride + voffset)
	and npos = idx_array.(idx * stride + noffset) in
	let vidx, new_counted = add_if_different
	  v_array.(vpos).x v_array.(vpos).y v_array.(vpos).z
	  (norm_accessor npos `x) (norm_accessor npos `y)
	  (norm_accessor npos `z) !counted in
	face.(idx) <- vidx;
	counted := new_counted
      done;
      let newpoints = match points with
	3 ->
	  (*  1
	     0 2 *)
	  face.(1) :: face.(0) :: face.(2) :: triangle_indices
      | 4 ->
	  (* 1 2
	     0 3 *)
	  face.(2) :: face.(0) :: face.(3)
	  :: face.(1) :: face.(0) :: face.(2) :: triangle_indices
      | 5 ->
          (*   2
	     1   3
	      0 4  *)
	  face.(2) :: face.(1) :: face.(0)
	  :: face.(4) :: face.(2) :: face.(0)
	  :: face.(3) :: face.(2) :: face.(4) :: triangle_indices
      | 7 ->
	  (*   3
	     2   4
	    1     5
	     0   6  *)
	  face.(2) :: face.(1) :: face.(0)
	  :: face.(2) :: face.(0) :: face.(3)
	  :: face.(3) :: face.(0) :: face.(6)
	  :: face.(4) :: face.(3) :: face.(6)
	  :: face.(5) :: face.(4) :: face.(6) :: triangle_indices
      | _ ->
	Printf.fprintf stderr
	  "Warning: found difficult polygon (%d sides)\n" points;
	  triangle_indices in
      newpoints, !counted
  | _ -> triangle_indices, !counted

let strip_geometries_alt geometries =
  let glist = geometry_list geometries in
  List.iter
    (fun geom ->
      let counted = ref []
      and triangle_indices = ref [] in
      List.iter
	(fun poly ->
	  if poly.geometry = geom then begin
	    let stride = poly_stride poly in
	    List.iter
	      (fun idx_array ->
	        match poly.poly_type with
		  Polygons ->
		    let new_tri_indices, new_counted
		      = add_polygons poly idx_array stride !triangle_indices
				     !counted in
		    triangle_indices := new_tri_indices;
		    counted := new_counted
		| Triangles ->
		    let num_tris = (Array.length idx_array) / (stride * 3) in
		    for tri = 0 to num_tris - 1 do
		      let slice = Array.sub idx_array (tri * stride * 3)
					    (stride * 3) in
		      let new_tri_indices, new_counted
		        = add_polygons poly slice stride !triangle_indices
				       !counted in
		      triangle_indices := new_tri_indices;
		      counted := new_counted
		    done
		| Polylist ->
		    ignore (List.fold_left
		      (fun from poly_len ->
		        let slice = Array.sub idx_array from
					      (poly_len * stride) in
			let new_tri_indices, new_counted
			  = add_polygons poly slice stride !triangle_indices
					      !counted in
			triangle_indices := new_tri_indices;
			counted := new_counted;
			from + poly_len * stride)
		      0
		      poly.vcount))
	      poly.polys;
	  end)
	geometries;
      let strips_out = Strips.run_strips (Array.of_list !triangle_indices) in
      let unique_arr = Array.of_list (List.rev !counted) in
      let fo = open_out "out.stp" in
      List.iter
        (fun slist ->
	  let rev_slist = List.rev slist in
	  Printf.fprintf fo "strip %d\n" (List.length slist);
	  Printf.fprintf fo "verts\n";
	  List.iter
	    (fun i ->
	      let (vx, vy, vz, _, _, _) = unique_arr.(i) in
	      Printf.fprintf fo "%f %f %f\n" vx vy vz)
	    rev_slist;
	  Printf.fprintf fo "norms\n";
	  List.iter
	    (fun i ->
	      let (_, _, _, nx, ny, nz) = unique_arr.(i) in
	      Printf.fprintf fo "%f %f %f\n" nx ny nz)
	    rev_slist;
	  Printf.fprintf fo "end\n")
	strips_out;
      close_out fo)
    glist


let strip_blank_data doc_root =
  let is_blank foo =
    let all_blank = ref true in
    String.iter (function
        ' ' | '\t' | '\n' | '\r' -> ()
      | _ -> all_blank := false)
      foo;
    !all_blank in
  Pxp_document.map_tree
    ~pre:(fun node ->
      match node#node_type with
        Pxp_document.T_data when is_blank node#data -> raise Pxp_document.Skip
      | _ -> node)
    doc_root

let _ =
  let config = Pxp_types.default_config
  and spec = Pxp_tree_parser.default_spec
  and source = Pxp_types.from_file Sys.argv.(1) in
  let dtd_source = Pxp_types.from_file "collada.auto.dtd" in
  let dtd = Pxp_dtd_parser.parse_dtd_entity config dtd_source in
  let doc = Pxp_tree_parser.parse_document_entity
	      ~transform_dtd:(fun _ -> dtd) config source spec in
  Pxp_document.strip_whitespace ~left:`Strip_seq ~right:`Strip_seq
				doc#root;
  let doc_root = strip_blank_data doc#root in
  (* scan_doc doc_root 0; *)
  let my_assets = get_assets doc_root in
  print_assets my_assets;
  get_library_geometries doc_root;
  get_library_images doc_root;
  get_library_effects doc_root;
  (* Fix up things we deferred on the first pass. *)
  List.iter (fun x -> x ()) !do_later;
  do_later := [];
  (* print_vertices vertices; *)
  (* print_geometries !geometries; *)
  strip_geometries_alt !geometries
