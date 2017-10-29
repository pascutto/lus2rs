type t = {
  id: int;
  name: string;
  kind: kind;
}

and kind =
  | Stream
  | Node
  | Prim

let make =
  let cpt = ref 0 in
  fun s kind ->
    incr cpt;
    {
      id = !cpt;
      name = s;
      kind = kind;
    }

let compare = Pervasives.compare

let print fmt x =
  Format.fprintf fmt "%s__%i" x.name x.id

let print_to_string print x =
  ignore (Format.flush_str_formatter ());
  print Format.str_formatter x;
  Format.fprintf Format.str_formatter "@?";
  Format.flush_str_formatter ()

let string_of x = print_to_string print x
