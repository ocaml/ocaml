(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Series = struct
  type t = {
    channel : out_channel;
    mutable closed : bool;
  }

  let create ~path =
    { channel = open_out path;
      closed = false;
    }

  external save_trie : out_channel -> unit
    = "caml_spacetime_only_works_for_native_code"
      "caml_spacetime_save_trie"

  let save_and_close t =
    if t.closed then failwith "Series is closed";
    save_trie t.channel;
    close_out t.channel;
    t.closed <- true
end

module Snapshot = struct

  external take : out_channel -> unit
    = "caml_spacetime_only_works_for_native_code"
      "caml_spacetime_take_snapshot"

  let take { Series.closed; channel } =
    if closed then failwith "Series is closed";
    take channel

end
