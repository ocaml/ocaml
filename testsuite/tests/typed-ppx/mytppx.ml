open Tast_mapper

let newmapper argv =
  {default with
     expr = (fun mapper expr ->
       match expr with
       | {exp_attributes = [{attr_name = {txt = "swap"}}];
          exp_desc = Texp_tuple [exp_a; exp_b];
          exp_type = {desc = Ttuple [typ_a; typ_b]} as typ_desc} ->
            if typ_a.desc = typ_b.desc then
              {expr with
                 exp_desc = Texp_tuple [exp_b; exp_a];
                 exp_type = {typ_desc with desc = Ttuple [typ_b; typ_a]}}
            else
              begin
                prerr_endline "Could not swap: types not alike";
                default.expr mapper expr
              end
       | _ -> default.expr mapper expr);
  }

let () =
  register "test" newmapper

