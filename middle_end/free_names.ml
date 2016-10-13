(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = {
  mutable free_variables : Variable.Set.t;
  mutable free_phantom_variables : Variable.Set.t;
  mutable free_symbols : Symbol.Set.t;
  mutable free_phantom_symbols : Symbol.Set.t;
}

type free_names = t

let is_free_variable t var = Variable.Set.mem var t.free_variables
let is_free_phantom_variable t var =
  Variable.Set.mem var t.free_phantom_variables

let free_variables t = t.free_variables
let free_phantom_variables t = t.free_phantom_variables
let all_free_variables t =
  Variable.Set.union (free_variables t) (free_phantom_variables t)

let free_symbols t = t.free_symbols
let free_phantom_symbols t = t.free_phantom_symbols
let all_free_symbols t =
  Symbol.Set.union (free_symbols t) (free_phantom_symbols t)

let subset t1 t2 =
  Variable.Set.subset t1.free_variables t2.free_variables
    && Variable.Set.subset t1.free_phantom_variables t2.free_phantom_variables
    && Symbol.Set.subset t1.free_symbols t2.free_symbols
    && Symbol.Set.subset t1.free_phantom_symbols t2.free_phantom_symbols

let print ppf t =
  Format.fprintf ppf "{ free_variables = %a;@ free_phantom_variables = %a;@ \
      free_symbols = %a;@ free_phantom_symbols = %a; }"
    Variable.Set.print t.free_variables
    Variable.Set.print t.free_phantom_variables
    Symbol.Set.print t.free_symbols
    Symbol.Set.print t.free_phantom_symbols

module Mutable = struct
  type nonrec t = t

  let create () =
    { free_variables = Variable.Set.empty;
      free_phantom_variables = Variable.Set.empty;
      free_symbols = Symbol.Set.empty;
      free_phantom_symbols = Symbol.Set.empty;
    }

  let free_variable t var =
    t.free_variables <- Variable.Set.add var t.free_variables

  let free_phantom_variable t var =
    t.free_phantom_variables <- Variable.Set.add var t.free_phantom_variables

  let free_symbol t sym =
    t.free_symbols <- Symbol.Set.add sym t.free_symbols

  let free_symbols t syms =
    t.free_symbols <- Symbol.Set.union syms t.free_symbols

  let free_phantom_symbol t sym =
    t.free_phantom_symbols <- Symbol.Set.add sym t.free_phantom_symbols

  let union_free_symbols_only t t' =
    t.free_symbols <- Symbol.Set.union t.free_symbols t'.free_symbols;
    t.free_phantom_symbols
      <- Symbol.Set.union t.free_phantom_symbols t'.free_phantom_symbols

  let union t t' =
    t.free_variables
      <- Variable.Set.union t.free_variables t'.free_variables;
    t.free_phantom_variables
      <- Variable.Set.union t.free_phantom_variables t'.free_phantom_variables;
    union_free_symbols_only t t'

  let bound_variables t bound_vars =
    t.free_variables <- Variable.Set.diff t.free_variables bound_vars;
    t.free_phantom_variables
      <- Variable.Set.diff t.free_phantom_variables bound_vars

  let freeze t =
    { free_variables = t.free_variables;
      free_phantom_variables = t.free_phantom_variables;
      free_symbols = t.free_symbols;
      free_phantom_symbols = t.free_phantom_symbols;
    }
end
