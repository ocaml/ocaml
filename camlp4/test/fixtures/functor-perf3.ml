<% types, with_constrs, make, make2 = ARGV.map { |x| x.to_i } %>

module type S = sig
<%- for i in 0 .. types do -%>
  type t<%= i %>
<%- end -%>
end

module Make (M : S)
: S with type t0 = M.t0
  <%- for i in 1 .. with_constrs do -%>
     and type t<%= i %> = M.t<%= i %>
  <%- end -%>
= struct
  include M
end

module type S2 = sig
  module M : S
end

module Make2 (M2 : S2)
: S2 with module M = M2.M
= struct
  include M2
end

module M = struct
<%- for i in 0 .. types do -%>
  type t<%= i %> = int -> int -> int
<%- end -%>
end

module M1 =
  Make
<%- make.times do -%>
  (Make
<%- end -%>
  (M)<%= ')' * make %>

module M2 = struct
  module M = M1
end

module X =
  Make2
<%- make2.times do -%>
  (Make2
<%- end -%>
  (M2)<%= ')' * make2 %>
