<% types, with_constrs, module_application = ARGV.map { |x| x.to_i } %>

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

module M = struct
<%- for i in 0 .. types do -%>
  type t<%= i %> = int -> int -> int
<%- end -%>
end

module X =
  Make
<%- module_application.times do -%>
  (Make
<%- end -%>
  (M)<%= ')' * module_application %>
