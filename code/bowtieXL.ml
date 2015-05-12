(* Burrows-Wheeler Transform string alignment without index compression.
 * Core algorithm adapted from: http://www.cs.jhu.edu/~langmea/resources/bwt_fm.pdf
 *)


(* utils *)

let fold_string f a s =
  let slen = String.length s in
  let rec loop a n =
    if n < slen
    then loop (f a s.[n]) (n+1)
    else a
  in
  loop a 0

let string_of_list ?(sep=" ") ?(border=(fun s -> "(" ^ s ^ ")")) soe l =
  if List.length l > 0 then
    let elts = List.fold_right (fun elt a -> (soe elt)^sep^a) l "" in
    border (String.sub elts 0 ((String.length elts)-(String.length sep)))
  else border ""

let string_of_map
    (type k)
    (type v)
    (type m)
    ?(sep     = ",")
    ?(border  = (fun s -> "[" ^ s ^ "]"))
    (fold : (k -> v -> 'a -> 'a) -> m -> 'a -> 'a)
    (sok : k -> string)
    (sov : v -> string)
    (m : m) : string =
  let binds, gt1 =
    fold
      (fun k v (b, g) ->
         Printf.sprintf "%s%s%s -> %s" b sep (sok k) (sov v), true)
      m
      ("", false)
  in
  let seplen = String.length sep in
  border
    (if gt1
     then String.sub binds seplen ((String.length binds) - seplen)
     else "")

let opt_map (type a) (type b) (f : a -> b) : a option -> b option =
  function
  | Some a -> Some (f a)
  | None   -> None

let time ?(prefix="") thunk =
  let t0  = Unix.gettimeofday () in
  let out = thunk () in
  let t   = Unix.gettimeofday () in
  print_endline (prefix^" took "^(string_of_float (t -. t0))^" s");
  out

let write_file (content : string) (path : string) : bool =
  let chan = open_out path in
  let out =
    try output_string chan content; flush chan; true
    with exc -> print_endline (Printexc.to_string exc); false in
  close_out chan;
  out && Sys.file_exists path



(* the guts: Burrows-Wheeler transform, inverse, alignment *)

let suffix_array (s : string) : int array =
  let slen = String.length s in
  let arr = Array.make (slen+1) 0 in
  let rec loop a i =
    if i <= slen
    then loop ((String.sub s i (slen - i), i)::a) (i+1)
    else List.sort (fun (s, _) (s', _) -> compare s s') a
  in
  List.iteri
    (fun i (_, n) -> arr.(i) <- n)
    (loop [] 0) ;
  arr

let bwt (s : string) (sa : int array) : string =
  Array.fold_left
    (fun a si ->
       if si = 0
       then a ^ "$"
       else a ^ (String.sub s (si-1) 1))
    ""
    sa

let first_col (tots : (char, int) Hashtbl.t) : (char, int * int) Hashtbl.t =
  let first : (char, int * int) Hashtbl.t = Hashtbl.create 10 in
  let sorted =
    List.sort (fun (c, _) (c', _) -> compare c c')
      (Hashtbl.fold
         (fun c n l -> (c, n)::l)
         tots
         [])
  in
  let _ =
    List.fold_left
      (fun tot (c, n) -> Hashtbl.replace first c (tot, tot+n) ; tot+n)
      0
      sorted
  in
  first

let rank_all (bws : string) : (char, int array) Hashtbl.t * (char, int) Hashtbl.t =
  let ranks : (char, int list) Hashtbl.t = Hashtbl.create 10 in
  let tots  : (char, int)      Hashtbl.t = Hashtbl.create 10 in
  String.iter
    (fun c ->
       if not (Hashtbl.mem tots c)
       then Hashtbl.replace tots c 0)
    bws ;
  String.iter
    (fun c ->
       Hashtbl.replace tots c ((Hashtbl.find tots c) + 1) ;
       Hashtbl.iter
         (fun c n ->
            let l = match Hashtbl.find ranks c with
              | l -> l
              | exception Not_found -> []
            in
            Hashtbl.replace ranks c (n::l))
         tots)
    bws ;
  let longest_list =
    Hashtbl.fold
      (fun _ l a ->
         let len = List.length l in
         if a > len then a else len)
      ranks
      0
  in
  let ranks_fast : (char, int array) Hashtbl.t = Hashtbl.create 10 in
  String.iter
    (fun c ->
       if not (Hashtbl.mem ranks_fast c)
       then Hashtbl.replace ranks_fast c (Array.make longest_list 0))
    bws ;
  Hashtbl.iter
    (fun c l ->
       let arr = Hashtbl.find ranks_fast c in
       List.iteri
         (fun i n -> arr.(i) <- n)
         (List.rev l))
    ranks ;
  (ranks_fast, tots)

let ibwt (bws : string) : string =
  let rank (bws : string) : int list * (char, int) Hashtbl.t =
    let slen = String.length bws in
    let tots = Hashtbl.create 10 in
    let rec loop ranks i =
      if i < slen
      then
        let c = bws.[i] in
        let count = match Hashtbl.find tots c with
          | count               -> count
          | exception Not_found -> 0
        in
        Hashtbl.replace tots c (count+1) ;
        loop (count::ranks) (i+1)
      else
        (List.rev ranks, tots)
    in
    loop [] 0
  in
  let ranks, tots = rank bws in
  let first = first_col tots in
  let rec loop s i =
    let c = bws.[i] in
    if c <> '$'
    then
      let s' = Printf.sprintf "%c%s" c s in
      let i' = fst (Hashtbl.find first c) + (List.nth ranks i) in
      loop s' i'
    else s
  in
  loop "" 0

let count_matches (bws : string) (p : string) : int =
  let ranks, tots = rank_all bws in
  let first = first_col tots in
  let plen = String.length p in
  let plast = p.[plen-1] in
  if not (Hashtbl.mem first plast)
  then 0
  else
    let rec loop l r i =
      if i >= 0 && r > 1
      then
        let c  = p.[i] in
        let   fstc = fst (Hashtbl.find first c) in
        let ranksc = Hashtbl.find ranks c in
        loop
          (fstc + (ranksc.(l-1)))
          (fstc + (ranksc.(r-1)))
          (i-1)
      else r-l
    in
    let l, r = Hashtbl.find first plast in
    let i = plen-2 in
    loop l r i

           (*   read   offsets  *)
type result = string * int list
let string_of_results =
  string_of_list ~sep:"\n" ~border:(fun s->s)
    (fun (r, off) ->
       Printf.sprintf "(%s, %s)"
         r
         (string_of_list ~sep:";" ~border:(fun s->"["^s^"]")
            string_of_int
            off))

        (* suffix array            ranks                      first col *)
type index = int array * (char, int array) Hashtbl.t * (char, int * int) Hashtbl.t

let create_index (genome : string) : index =
  let suffarr = suffix_array genome in
  let bwtg = bwt genome suffarr in
  let ranks, tots = rank_all bwtg in
  let first = first_col tots in
  (suffarr, ranks, first)

let align_read ((suffarr, ranks, first) : index) (read : string) : result =
  let fail = (read, []) in
  if read = "" then fail
  else
    try
      let plen = String.length read in
      let plast = read.[plen-1] in
      if not (Hashtbl.mem first plast)
      then fail
      else
        let rec loop l r i =
          if i >= 0 && r > 1
          then
            let c  = read.[i] in
            let   fstc = fst (Hashtbl.find first c) in
            let ranksc = Hashtbl.find ranks c in
            loop
              (fstc + (ranksc.(l-1)))
              (fstc + (ranksc.(r-1)))
              (i-1)
          else (l, r)
        in
        let l,  r   = Hashtbl.find first plast in
        let l', r'  = loop l r (plen-2) in
        let offsets =
          let rec loop a n =
            if n < r'
            then loop ((suffarr.(n))::a) (n+1)
            else a
          in
          loop [] l'
        in
        (read, offsets)
    with Not_found -> fail

let align (genome : string) : string list -> result list =
  List.map (align_read (create_index genome))

let index_align (index : index) (reads : string list) : result list =
  let  len = List.length reads in
  let flen = float_of_int len in
  let print_progress i =
    Printf.printf "\r%i / %i complete (%i%%)%!"
      i
      len
      (int_of_float (floor (100. *. (float_of_int i) /. flen))) ;
  in
  let align = align_read index in
  time ~prefix:"Alignment"
    (fun () ->
       let out =
         List.mapi
           (fun i read ->
              (if i mod 50 = 0 then print_progress i) ;
              align read)
           reads
       in
       print_progress len ; print_newline () ;
       out)


(* high-level entry points *)

let parse_file (type a) par path : a option =
  let lexbuf = Lexing.from_channel (open_in path) in
  try
    Some (par Lexer.token lexbuf)
  with
    | exn ->
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      Printf.eprintf "File %s, line %d, character %d: syntax error at %s\n%!"
        path line cnum
        (if tok = "\n" then
            "newline"
         else Printf.sprintf "`%s'" tok);
      None

let read_reads (path : string) : (string * string * string * string) list option =
  parse_file Parser.fasta path

let read_genome (path : string) : (string * string) option =
  parse_file Parser.fastq path

let create_index  : string -> string -> unit =
  fun genome output ->
    match read_genome genome with
    | None   -> failwith ("Failed to read the given genome: " ^ genome)
    | Some (desc, seq) ->
      let sEQ = String.uppercase seq in
      let index = create_index sEQ in
      try
        let chan = open_out output in
        Marshal.to_channel chan index [] ;
        flush chan ;
        close_out chan ;
        print_endline ("Successfully wrote index to " ^ output)
      with _ -> failwith ("Failed to write index to file: " ^ output)

let run_alignment : string -> string -> string -> unit =
  fun index reads output ->
    let index : index =
      try
        let chan = open_in index in
        let index : index = Marshal.from_channel chan in
        close_in chan ;
        index
      with exc ->
        failwith ("Failed to read the given index: " ^ index ^ "\n" ^
                  (Printexc.to_string exc))
    in
    match read_reads reads with
    | None -> failwith ("Failed to read the given reads: " ^ reads)
    | Some ss ->
      try
        let ress =
          index_align
            index
            (List.map (fun (_,read,_,_) -> String.uppercase read) ss)
        in
        print_endline "Writing reads to file." ;
        if write_file (string_of_results ress) output
        then print_endline ("Successfully aligned reads. Output: " ^ output)
        else print_endline ("Failed to write read alignments to file: " ^ output)
      with exc -> failwith ("Failed to align reads:\n" ^ (Printexc.to_string exc))



(* main *)

let main () =
  let this_bin_name =
    let name = Sys.executable_name in
    let len  = String.length name in
    let ind  = String.rindex_from name (len-1) '/' in
    String.sub name (ind+1) (len-ind-1)
  in
  let usage =
    Printf.sprintf
      ("Usage: ./%s -index -genome genome.fa -output index.dex\n" ^^
       "       ./%s -align -index  index.dex -reads  reads.fq -output results.txt")
      this_bin_name
      this_bin_name
  in
  let index        = ref false
  and index_genome = ref ""
  and align        = ref false
  and align_index  = ref ""
  and align_reads  = ref ""
  and output       = ref ""
  in
  let spec = ref [] in
  let index_spec =
    [ ("-genome", Arg.Set_string index_genome, "file-path -- FASTA formatted genome to be indexed")
    ; ("-output", Arg.Set_string output,       "file-path -- Write the index to this location")
    ]
  and align_spec =
    [ ("-index",  Arg.Set_string align_index,  "file-path -- Location of precomputed genome index")
    ; ("-reads",  Arg.Set_string align_reads,  "file-path -- FASTQ formatted short reads")
    ; ("-output", Arg.Set_string output,       "file-path -- Write the alignments to this location")
    ]
  in
  let initial_spec =
    [ ("-align",  Arg.Unit (fun _ -> align := true ; spec := align_spec),
       "-- Align short reads to a precomputed index")
    ; ("-index",  Arg.Unit (fun _ -> index := true ; spec := index_spec),
       "-- Create index of a genome")
    ]
  in
  spec := initial_spec ;
  Arg.parse_dynamic
    spec
    (fun anon_arg -> failwith ("Unexpected anonymous argument: " ^ anon_arg))
    usage ;
  try
    (if !index && !index_genome <> "" && !output <> ""
     then create_index !index_genome !output
     else if !align && !align_index <> "" && !align_reads <> "" && !output <> ""
     then run_alignment !align_index !align_reads !output
     else (print_endline usage ; exit 1)) ;
    exit 0
  with Failure msg -> print_endline msg ; exit 1

let _ = main ()
