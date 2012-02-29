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
    mutable colour : (string * int) option;
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
    mutable transparency : float option;
    mutable index_of_refraction : float option
  }

and lambert =
  {
    mutable l_ambient : colour option;
    mutable l_diffuse : covering option;
    mutable l_emission : colour option;
    mutable l_index_of_refraction : float option
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

(* Whee, an ugly global.  *)
let flip_yz = ref false

let make_accessor source_id array_source count stride param_list =
  let rec scan_params acc plist pnum =
    match plist with
      [] -> acc
    | (ptype, pname)::ps ->
        if ptype <> Float_param then
	  failwith "Non-float accessor not implemented";
        let tag = begin match pname with
	  "X" -> `x
	| "Y" -> if !flip_yz then `z else `y
	| "Z" -> if !flip_yz then `y else `z
	| "S" -> `s | "T" -> `t
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

(* This is a bit obfuscated because we want to avoid building a whole pile of
   exception handlers on the stack.  *)
let parse_int_list datastring =
  let lexer = Genlex.make_lexer [] (Stream.of_string datastring) in
  let rec accumulate entries =
    let item =
      try
	let item = Stream.next lexer in
	match item with
	  Genlex.Int n -> Some n
	| _ -> raise (Bad_token item)
      with Stream.Failure -> None in
    match item with
      Some n -> accumulate (n :: entries)
    | None -> entries in
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
      index_of_refraction = None
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
      | Pxp_document.T_element "index_of_refraction" ->
          phonginf.index_of_refraction <- Some (parse_float (node_child node))
      | _ -> bad_node node "below phong")
    phong_parts

let parse_lambert id lambert_parts =
  let lambertinf =
    {
      l_ambient = None;
      l_diffuse = None;
      l_emission = None;
      l_index_of_refraction = None
    } in
  List.iter
    (fun node ->
      match node#node_type with
        Pxp_document.T_element "ambient" ->
	  lambertinf.l_ambient <- Some (parse_colour (node_child node))
      | Pxp_document.T_element "diffuse" ->
          lambertinf.l_diffuse <- Some (parse_covering (node_child node))
      | Pxp_document.T_element "emission" ->
          lambertinf.l_emission <- Some (parse_colour (node_child node))
      | Pxp_document.T_element "index_of_refraction" ->
	  lambertinf.l_index_of_refraction
	    <- Some (parse_float (node_child node))
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
      | Pxp_document.T_element "extra" ->
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
  | Colour

let parse_input input_node =
  let source = resolve_local_ref (input_node#required_string_attribute "source")
  and offset = input_node#attribute "offset" in
  let sem = match input_node#required_string_attribute "semantic" with
    "POSITION" -> Position
  | "VERTEX" -> Vertex
  | "NORMAL" -> Normal
  | "TEXCOORD" -> Texcoord
  | "UV" -> UV
  | "COLOR" -> Colour
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
  and colour = ref None
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
	  | Colour -> colour := Some (source, ioffset)
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
      colour = !colour;
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
      | Pxp_document.T_element "extra" ->
         (* Ignore *)
	 ()
      | _ -> bad_node prof_com_part "below profile_common")
    profile_common_parts

let parse_effect id effect_nodes =
  List.iter
    (fun effect_part ->
      match effect_part#node_type with
        Pxp_document.T_element "profile_COMMON" ->
	  parse_profile_common id effect_part#sub_nodes
      | Pxp_document.T_element "extra" ->
          (* Ignore *)
	  ()
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

let colour_offset poly =
  match poly.colour with
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
  increase_to colour_offset;
  succ !highest

(* I am going straight to programmer hell for this.  *)

let material_to_texture = function
    "cube11_copy15_au" -> 1
  | "cube11_copy13_au" -> 2
  | "cube11_copy14_au" -> 3
  | _ -> 0

let print_geometries geometries =
  List.iter
    (fun poly ->
      let stride = poly_stride poly in
      Printf.printf "For geometry %s:\n" poly.geometry;
      let mat, tex = match poly.material with
        Some x -> x, material_to_texture x
      | None -> "(not set)", -1 in
      Printf.printf "Using hardwired texture %d for material '%s'\n"
        tex mat;
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

let add_if_different vx vy vz nx ny nz tu tv counted =
  try
    Hashtbl.find counted (vx, vy, vz, nx, ny, nz, tu, tv)
  with Not_found ->
    let size = Hashtbl.length counted in
    Hashtbl.add counted (vx, vy, vz, nx, ny, nz, tu, tv) size;
    size

(*
let add_if_different vx vy vz nx ny nz tu tv counted =
  let rec scan idx = function
    (vx', vy', vz', nx', ny', nz', tu', tv') :: more ->
      Hashtbl.find counted
      if abs_float (vx' -. vx) <= epsilon
         && abs_float (vy' -. vy) <= epsilon
	 && abs_float (vz' -. vz) <= epsilon
	 && abs_float (nx' -. nx) <= epsilon
	 && abs_float (ny' -. ny) <= epsilon
	 && abs_float (nz' -. nz) <= epsilon
	 && abs_float (tu' -. tu) <= epsilon
	 && abs_float (tv' -. tv) <= epsilon then
	idx
      else
        scan (idx - 1) more
  | _ -> raise Not_found in
  try
    let idx = scan (List.length counted - 1) counted in
    idx, counted
  with Not_found ->
    let idx = List.length counted in
    idx, (vx, vy, vz, nx, ny, nz, tu, tv) :: counted
*)

let have_texcoords = ref false

let invert_poly = ref false

let add_tri pt1 pt2 pt3 indices =
  if !invert_poly then
    pt2 :: pt1 :: pt3 :: indices
  else
    pt1 :: pt2 :: pt3 :: indices

let add_polygons poly idx_array stride triangle_indices counted =
  let points = (Array.length idx_array) / stride in
  let fetch_texcoords =
    match poly.texcoord with
      Some (tname, toffset) ->
	let _, _, texc_accessor = Hashtbl.find accessors tname in
	have_texcoords := true;
	fun idx ->
	  let tpos = idx_array.(idx * stride + toffset) in
	  (texc_accessor tpos `s), (texc_accessor tpos `t)
    | None -> fun idx -> 0.0, 0.0 in
  match poly.vertex, poly.normal with
    Some (vname, voffset), Some (nname, noffset) ->
      let v_array = Hashtbl.find vertices vname in
      let _, _, norm_accessor = Hashtbl.find accessors nname in
      let face = Array.create points (-1) in
      for idx = 0 to points - 1 do
	let vpos = idx_array.(idx * stride + voffset)
	and npos = idx_array.(idx * stride + noffset) in
	let s, t = fetch_texcoords idx in
	let vidx = add_if_different
	  v_array.(vpos).x v_array.(vpos).y v_array.(vpos).z
	  (norm_accessor npos `x) (norm_accessor npos `y)
	  (norm_accessor npos `z) s t counted in
	face.(idx) <- vidx
      done;
      let newpoints = match points with
	3 ->
	  (*  1
	     0 2 *)
	  add_tri face.(1) face.(0) face.(2) triangle_indices
      | 4 ->
	  (* 1 2
	     0 3 *)
	  add_tri face.(2) face.(0) face.(3)
	    (add_tri face.(1) face.(0) face.(2) triangle_indices)
      | 5 ->
          (*   2
	     1   3
	      0 4  *)
	  add_tri face.(2) face.(1) face.(0)
	    (add_tri face.(4) face.(2) face.(0)
	    (add_tri face.(3) face.(2) face.(4) triangle_indices))
      | 6 ->
          (*  2 3
	     1   4
	      0 5  *)
	  add_tri face.(2) face.(1) face.(0)
	    (add_tri face.(3) face.(2) face.(0)
	    (add_tri face.(5) face.(3) face.(0)
	    (add_tri face.(4) face.(3) face.(5) triangle_indices)))
      | 7 ->
	  (*   3
	     2   4
	    1     5
	     0   6  *)
	  add_tri face.(2) face.(1) face.(0)
	    (add_tri face.(2) face.(0) face.(3)
	    (add_tri face.(3) face.(0) face.(6)
	    (add_tri face.(4) face.(3) face.(6)
	    (add_tri face.(5) face.(4) face.(6) triangle_indices))))
      | _ ->
	Printf.fprintf stderr
	  "Warning: found difficult polygon (%d sides)\n" points;
	  triangle_indices in
      newpoints
  | _ -> triangle_indices

let emit_stripsfile fo strips unique_arr use_texture =
  List.iter
   (fun slist ->
     let rev_slist = List.rev slist in
     Printf.fprintf fo "strip %d\n" (List.length slist);
     begin match use_texture with
       Some tex -> Printf.fprintf fo "texidx %d\n" tex
     | None -> ()
     end;
     Printf.fprintf fo "verts\n";
     List.iter
       (fun i ->
	 let (vx, vy, vz, _, _, _, _, _) = unique_arr.(i) in
	 Printf.fprintf fo "%f %f %f\n" vx vy vz)
       rev_slist;
     Printf.fprintf fo "norms\n";
     List.iter
       (fun i ->
	 let (_, _, _, nx, ny, nz, _, _) = unique_arr.(i) in
	 Printf.fprintf fo "%f %f %f\n" nx ny nz)
       rev_slist;
     if !have_texcoords then begin
       Printf.fprintf fo "texcoords\n";
       List.iter
	 (fun i ->
	   let (_, _, _, _, _, _, tu, tv) = unique_arr.(i) in
	   Printf.fprintf fo "%f %f\n" tu tv)
	 rev_slist
     end;
     Printf.fprintf fo "end\n")
   strips

let array_of_point_hash counted =
  let arr = Array.create (Hashtbl.length counted)
			 (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0) in
  Hashtbl.iter
    (fun pt idx ->
      arr.(idx) <- pt)
    counted;
  arr

let strip_geometries_alt geometries outfile =
  have_texcoords := false;
  let glist = geometry_list geometries in
  Printf.printf "Opening '%s' for output\n" outfile;
  let fo = open_out outfile in
  List.iter
    (fun geom ->
      Printf.printf "Found geometry '%s'\n" geom;
      let counted = Hashtbl.create 10
      and triangle_indices = ref []
      and use_texture = ref None in
      List.iter
	(fun poly ->
	  if poly.geometry = geom then begin
	    let mat, tex = match poly.material with
	      Some x -> x, material_to_texture x
	    | None -> "(not set)", -1 in
	    Printf.printf "Using hardwired texture %d for material '%s'\n"
	      tex mat;
	    if tex != -1 then begin
	      use_texture := Some tex
	    end;
	    let stride = poly_stride poly in
	    List.iter
	      (fun idx_array ->
	        match poly.poly_type with
		  Polygons ->
		    let new_tri_indices
		      = add_polygons poly idx_array stride !triangle_indices
				     counted in
		    triangle_indices := new_tri_indices
		| Triangles ->
		    let num_tris = (Array.length idx_array) / (stride * 3) in
		    for tri = 0 to num_tris - 1 do
		      let slice = Array.sub idx_array (tri * stride * 3)
					    (stride * 3) in
		      let new_tri_indices
		        = add_polygons poly slice stride !triangle_indices
				       counted in
		      triangle_indices := new_tri_indices
		    done
		| Polylist ->
		    ignore (List.fold_left
		      (fun from poly_len ->
		        let slice = Array.sub idx_array from
					      (poly_len * stride) in
			let new_tri_indices
			  = add_polygons poly slice stride !triangle_indices
					      counted in
			triangle_indices := new_tri_indices;
			from + poly_len * stride)
		      0
		      poly.vcount))
	      poly.polys;
	  end)
	geometries;
      Printf.fprintf stderr "Tristripping...\n"; flush stderr;
      let strips_out = Strips.run_strips (Array.of_list !triangle_indices) in
      Printf.fprintf stderr "Uniquifying...\n"; flush stderr;
      let unique_arr = array_of_point_hash counted in
      Printf.fprintf stderr "Generating output...\n"; flush stderr;
      emit_stripsfile fo strips_out unique_arr !use_texture)
    glist;
  close_out fo

(* Add POINT to COUNTED if none of the existing points satisfy CLOSE_P.
   Otherwise return the index of an existing point, and the original list.  *)

let uniquify_point close_p point counted =
  let rec scan idx = function
    (point') :: more ->
      if close_p point point' then
        idx
      else
        scan (idx - 1) more
   | [] -> raise Not_found in
   try
     let idx = scan (List.length counted - 1) counted in
     idx, counted
   with Not_found ->
     let idx = List.length counted in
     idx, point :: counted

let epsilon = 0.000000001

let texc_equalish (s, t) (s', t') =
  abs_float (s' -. s) <= epsilon && abs_float (t' -. t) <= epsilon

let unique_index item counted =
  try
    Hashtbl.find counted item
  with Not_found ->
    let size = Hashtbl.length counted in
    Hashtbl.add counted item size;
    size

let points_equalish (x, y, z) (x', y', z') =
  abs_float (x -. x') <= epsilon
  && abs_float (y -. y') <= epsilon
  && abs_float (z -. z') <= epsilon

(* FUNC:
      int list list  (list of strip indices)
      -> (float * float * float * float * float * float * float * float) array
      -> 'a option
   GEOMETRIES:
      Objconvert.polygon list
   ACC:
     'b
*)

let fold_geometry_strips func geometries glist acc =
  have_texcoords := false;
  List.fold_right
    (fun geom acc ->
      let counted = Hashtbl.create 10
      and triangle_indices = ref []
      and use_texture = ref None in
      List.iter
	(fun poly ->
	  if poly.geometry = geom then begin
	    let stride = poly_stride poly in
	    List.iter
	      (fun idx_array ->
	        match poly.poly_type with
		  Polygons ->
		    let new_tri_indices
		      = add_polygons poly idx_array stride !triangle_indices
				     counted in
		    triangle_indices := new_tri_indices
		| Triangles ->
		    let num_tris = (Array.length idx_array) / (stride * 3) in
		    for tri = 0 to num_tris - 1 do
		      let slice = Array.sub idx_array (tri * stride * 3)
					    (stride * 3) in
		      let new_tri_indices
		        = add_polygons poly slice stride !triangle_indices
				       counted in
		      triangle_indices := new_tri_indices
		    done
		| Polylist ->
		    ignore (List.fold_left
		      (fun from poly_len ->
		        let slice = Array.sub idx_array from
					      (poly_len * stride) in
			let new_tri_indices
			  = add_polygons poly slice stride !triangle_indices
					      counted in
			triangle_indices := new_tri_indices;
			from + poly_len * stride)
		      0
		      poly.vcount))
	      poly.polys;
	  end)
	geometries;
      Printf.fprintf stderr "Tristripping...\n";
      let strips_out = Strips.run_strips (Array.of_list !triangle_indices) in
      Printf.fprintf stderr "Uniquifying...\n";
      let unique_arr = array_of_point_hash counted in
      Printf.fprintf stderr "Generating output...\n";
      func strips_out unique_arr !use_texture acc)
    glist
    acc

let print_vec3_list fo name vl =
  Printf.fprintf fo "f32 %s[] ATTRIBUTE_ALIGN(32) = {\n" name;
  List.iter
    (fun (x, y, z) ->
      Printf.fprintf fo "  %f, %f, %f,\n" x y z)
    vl;
  Printf.fprintf fo "};\n"

let print_vec2_list fo name vl =
  Printf.fprintf fo "f32 %s[] ATTRIBUTE_ALIGN(32) = {\n" name;
  List.iter
    (fun (x, y) ->
      Printf.fprintf fo "  %f, %f,\n" x y)
    vl;
  Printf.fprintf fo "};\n"

let print_strips fo name strips ~nbt =
  ignore (List.fold_right
    (fun strip num ->
      Printf.fprintf fo "static u16 %s_%d[] = {\n" name num;
      List.iter
	(fun (pi, ni, bni, tni, ti) ->
	  if nbt then
	    Printf.fprintf fo "  %d, %d, %d, %d, %d,\n" pi ni bni tni ti
	  else
	    Printf.fprintf fo "  %d, %d, %d,\n" pi ni ti)
	(List.rev strip);
      Printf.fprintf fo "};\n\n";
      succ num)
    strips
    0)

let print_strip_lengths fo name strips =
  Printf.fprintf fo "unsigned int %s[] = {\n" name;
  ignore (List.fold_right
    (fun strip num ->
      Printf.fprintf fo "  %d,\n" (List.length strip);
      succ num)
    strips
    0);
  Printf.fprintf fo "};\n"

let print_strip_ptrs fo name s_name strips =
  Printf.fprintf fo "u16 *%s[] = {\n" name;
  ignore (List.fold_right
    (fun strip num ->
      Printf.fprintf fo "  %s_%d,\n" s_name num;
      succ num)
    strips
    0);
  Printf.fprintf fo "};\n"

let get_pos coord_arr idx =
  let (px, py, pz, _, _, _, _, _) = coord_arr.(idx) in px, py, pz

let get_norm coord_arr idx =
  let (_, _, _, nx, ny, nz, _, _) = coord_arr.(idx) in nx, ny, nz

let get_texcoord coord_arr idx =
  let (_, _, _, _, _, _, ts, tt) = coord_arr.(idx) in ts, tt

let vec_scale s (x, y, z) =
  s *. x, s *. y, s *. z

let vec_add (ax, ay, az) (bx, by, bz) =
  ax +. bx, ay +. by, az +. bz

let vec_sub (ax, ay, az) (bx, by, bz) =
  ax -. bx, ay -. by, az -. bz

let vec_cross (ax, ay, az) (bx, by, bz) =
  ay *. bz -. az *. by,
  az *. bx -. ax *. bz,
  ax *. by -. ay *. bx

let vec_dot (ax, ay, az) (bx, by, bz) =
  ax *. bx +. ay *. by +. az *. bz

let vec_norm (x, y, z) =
  let len = sqrt (x *. x +. y *. y +. z *. z) in
  if len = 0.0 then
    0.0, 0.0, 0.0
  else
    let rlen = 1.0 /. len in
    x *. rlen, y *. rlen, z *. rlen

let inv_or_zero n =
  if n = 0.0 then 0.0 else 1.0 /. n

let degenerate_tri coord_arr ai bi ci =
  let pa = get_pos coord_arr ai
  and pb = get_pos coord_arr bi
  and pc = get_pos coord_arr ci
  and ta = get_texcoord coord_arr ai
  and tb = get_texcoord coord_arr bi
  and tc = get_texcoord coord_arr ci in
  let points = Hashtbl.create 3
  and texcs = Hashtbl.create 3 in
  ignore (unique_index pa points);
  ignore (unique_index pb points);
  ignore (unique_index pc points);
  ignore (unique_index ta texcs);
  ignore (unique_index tb texcs);
  ignore (unique_index tc texcs);
  Hashtbl.length points != 3 || Hashtbl.length texcs != 3

let vec_nonzero (vx, vy, vz) =
  vx <> 0.0 || vy <> 0.0 || vz <> 0.0

(* Attempt to fill in gaps for binormals/tangents which couldn't be calculated
   due to degenerate triangles in strips.  *)

let fill_degenerate_gaps u_arr v_arr coord_arr strip_arr degenerates =
  List.iter
    (fun i ->
      (*Printf.fprintf stderr "Attempting to fill %d\n" i;*)
      let pos = get_pos coord_arr strip_arr.(i)
      and texc = get_texcoord coord_arr strip_arr.(i) in
      for j = 0 to Array.length strip_arr - 1 do
        let opos = get_pos coord_arr strip_arr.(j)
	and otexc = get_texcoord coord_arr strip_arr.(j) in
	if points_equalish pos opos && texc_equalish texc otexc then begin
	  if vec_nonzero u_arr.(j) then begin
	    (*Printf.fprintf stderr "set u at %d from %d\n" i j;*)
	    u_arr.(i) <- u_arr.(j)
	  end;
	  if vec_nonzero v_arr.(j) then begin
	    (*Printf.fprintf stderr "set v at %d from %d\n" i j;*)
	    v_arr.(i) <- v_arr.(j)
	  end
	end
      done)
    degenerates

exception Degenerate

let get_uv_from_tri coord_arr ai bi ci =
  let a = get_pos coord_arr ai
  and b = get_pos coord_arr bi
  and c = get_pos coord_arr ci in
  let e = vec_sub b a
  and f = vec_sub c a in
  let s0, t0 = get_texcoord coord_arr ai
  and s1, t1 = get_texcoord coord_arr bi
  and s2, t2 = get_texcoord coord_arr ci in
  if degenerate_tri coord_arr ai bi ci then
    raise Degenerate;
  let u_num = vec_sub (vec_scale (t2 -. t0) e) (vec_scale (t1 -. t0) f)
  and v_num = vec_sub (vec_scale (s1 -. s0) f) (vec_scale (s2 -. s0) e) in
  let denom = (s1 -. s0) *. (t2 -. t0) -. (s2 -. s0) *. (t1 -. t0) in
  let u = vec_scale (inv_or_zero denom) u_num
  and v = vec_scale (inv_or_zero denom) v_num in
  let orig_norm = get_norm coord_arr ai in
  (* Using the original normal lets us do better for curved surfaces.  *)
  if false then
    vec_norm (vec_cross v orig_norm), vec_norm (vec_cross orig_norm u)
  else
    vec_norm u, vec_norm v

let calculate_nbt coord_arr strip =
  let strip_arr = Array.of_list (List.rev strip) in
  let strip_len = Array.length strip_arr in
  let u_arr = Array.make strip_len (0.0, 0.0, 0.0)
  and v_arr = Array.make strip_len (0.0, 0.0, 0.0) in
  let degenerates = ref [] in
  for i = 0 to strip_len - 1 do
    (* Get a "clockwise" triangle, first - i - second.  *)
    let first, second =
      if i == 0 then
        1, 2
      else if i == strip_len - 1 then
        strip_len - 3, strip_len - 2
      else
        i - 1, i + 1 in
    let first', second' =
      if i land 1 == 0 then
        second, first
      else
        first, second in
    (* ...so this is a triangle BAC.  *)
    let ai = strip_arr.(i)
    and bi = strip_arr.(first')
    and ci = strip_arr.(second') in
    begin try
      let u, v = get_uv_from_tri coord_arr ai bi ci in
      u_arr.(i) <- u;
      v_arr.(i) <- v
    with Degenerate ->
      if i > 0 && i < strip_len - 2 then begin
        (*Printf.fprintf stderr "Trying mid-strip alternative\n";*)
	let first, second =
	  if i land 1 == 0 then
	    i + 2, i + 1
	  else
	    i + 1, i + 2 in
	let bi = strip_arr.(first)
	and ci = strip_arr.(second) in
	try
	  let u, v = get_uv_from_tri coord_arr ai bi ci in
	  (*Printf.fprintf stderr "Mid-strip alternative successful!\n";*)
	  u_arr.(i) <- u;
	  v_arr.(i) <- v
	with Degenerate ->
          Printf.fprintf stderr
	    "%d is degenerate! (Even after trying alternative)\n" i;
	  degenerates := i :: !degenerates
      end else begin
	(*Printf.fprintf stderr "%d is degenerate!\n" i;*)
	degenerates := i :: !degenerates
      end
    end
  done;
  fill_degenerate_gaps u_arr v_arr coord_arr strip_arr !degenerates;
  u_arr, v_arr

let wrap idx =
  if idx < 0 then idx + 65536 else idx

let list_of_pos_norm counted =
  let arr = Array.create (Hashtbl.length counted) (0.0, 0.0, 0.0) in
  Hashtbl.iter
    (fun pt idx ->
      arr.(idx) <- pt)
    counted;
  Array.to_list arr

let list_of_texc counted =
  let arr = Array.create (Hashtbl.length counted) (0.0, 0.0) in
  Hashtbl.iter
    (fun pt idx ->
      arr.(idx) <- pt)
    counted;
  Array.to_list arr

let geometry_to_gx fo name geometries ~select_objects ~nbt =
  let pos = Hashtbl.create 10
  and norm = Hashtbl.create 10
  and tx = Hashtbl.create 10 in
  let glist = if select_objects = [] then
    geometry_list geometries
  else begin
    select_objects
  end in
  let reindexed_strips =
    fold_geometry_strips
      (fun strip_list coord_arr use_texture acc ->
        List.fold_right
	  (fun strip strips ->
	    let u_arr, v_arr =
	      if nbt then
	        calculate_nbt coord_arr strip
	      else
	        Array.make 0 (0.0, 0.0, 0.0), Array.make 0 (0.0, 0.0, 0.0) in
	    let one_strip, _ =
	      List.fold_right
		(fun idx (out_strip, offset) ->
	          let (px, py, pz, nx, ny, nz, ts, tt) = coord_arr.(idx) in
		  let pidx = unique_index (px, py, pz) pos
		  and nidx = unique_index (nx, ny, nz) norm
		  and tidx = unique_index (ts, tt) tx in
		  let bindx, tandx =
		    if nbt then
		      let bindx = unique_index u_arr.(offset) norm in
		      let tandx = unique_index v_arr.(offset) norm in
		      wrap (bindx - 1), wrap (tandx - 2)
		    else
		      -1, -1 in
		  ((pidx, nidx, bindx, tandx, tidx) :: out_strip), succ offset)
		strip
		([], 0) in
	      (one_strip :: strips))
	  strip_list
	  acc)
      geometries
      glist
      [] in
  print_vec3_list fo (name ^ "_pos") (list_of_pos_norm pos);
  Printf.fprintf fo "\n";
  print_vec3_list fo (name ^ "_norm") (list_of_pos_norm norm);
  Printf.fprintf fo "\n";
  print_vec2_list fo (name ^ "_texidx") (list_of_texc tx);
  Printf.fprintf fo "\n";
  print_strips fo (name ^ "_strip") reindexed_strips ~nbt;
  print_strip_lengths fo (name ^ "_lengths") reindexed_strips;
  Printf.fprintf fo "\n";
  print_strip_ptrs fo (name ^ "_strips") (name ^ "_strip") reindexed_strips

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
  let outfile = ref ""
  and infile = ref ""
  and geom_name = ref ""
  and generate_c = ref false
  and selected_objects = ref []
  and list_geom = ref false
  and gen_binormal_tangent = ref false in
  let add_selection obj =
    selected_objects := obj :: !selected_objects in
  let argspec =
    ["-o", Arg.Set_string outfile, "Set output file (file.strips)";
     "-n", Arg.Set_string geom_name, "Set geometry name";
     "-c", Arg.Set generate_c, "Generate C source";
     "-yz", Arg.Set flip_yz, "Swap Y/Z coordinates";
     "-i", Arg.Set invert_poly, "Inside-out polygons (use with -yz)";
     "-s", Arg.String add_selection, "Select geometry for inclusion in output";
     "-l", Arg.Set list_geom, "Output list of geometries";
     "-t", Arg.Set gen_binormal_tangent, "Generate binormals & tangents"]
  and usage = "Usage: objconvert [options] infile -o outfile" in
  Arg.parse argspec (fun name -> infile := name) usage;
  if not !list_geom && (!infile = "" || !outfile = "") then begin
    if !infile = "" then
      prerr_endline "Input file missing."
    else
      prerr_endline "Output file missing.";
    Arg.usage argspec usage;
    exit 1
  end;
  if !geom_name = "" then
    geom_name := Filename.chop_extension (Filename.basename !outfile);
  let config = Pxp_types.default_config
  and spec = Pxp_tree_parser.default_spec
  and source, dest =
    try
      Pxp_types.from_file !infile, !outfile
    with x ->
      prerr_endline "Parse error:";
      raise x in
  let dtd_filename = Filename.concat (Filename.dirname Sys.argv.(0))
		       "collada.auto.dtd" in
  let dtd_source = Pxp_types.from_file dtd_filename in
  Printf.fprintf stderr "Parsing...\n"; flush stderr;
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
  if !list_geom then begin
    let glist = geometry_list !geometries in
    Printf.fprintf stderr "Geometries in input file:\n";
    List.iter
      (fun geom -> Printf.fprintf stderr "  %s\n" geom)
      glist;
    exit 0
  end;
  if !generate_c then begin
    let fo = open_out !outfile in
    Printf.fprintf stderr "Converting to GX format...\n"; flush stderr;
    geometry_to_gx fo !geom_name !geometries ~select_objects:!selected_objects
		   ~nbt:!gen_binormal_tangent;
    close_out fo
  end else begin
    Printf.fprintf stderr "Converting to strips file...\n";
    strip_geometries_alt !geometries dest
  end
