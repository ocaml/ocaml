# system "cat /tmp/MetaAst.ml" ; exit

# Beware this script is a Q&D <:meta< >> expander.
# example:
#   header
#   <:start_meta< foo, bar >>
#       ...
#       match x with
#       | <:meta< <:expr< f x >> >>
#       ... meta_kind ...
#   <:stop_meta<>>
#   footer
# gives:
#   header
#   
#     module Foo = struct
#       ...
#         match x with
#         | <:expr@loc< f x >> -> <:foo< Ast.ExApp $meta_loc_foo loc$ (Ast.ExId $meta_loc_foo loc$ (Ast.IdLid $meta_loc_foo loc$ "f"))
#     (Ast.ExId $meta_loc_foo loc$ (Ast.IdLid $meta_loc_foo loc$ "x")) >>
#         ... foo ...
#     end;
#   
#     module Bar = struct
#       ...
#         match x with
#         | <:expr@loc< f x >> -> <:bar< Ast.ExApp $meta_loc_bar loc$ (Ast.ExId $meta_loc_bar loc$ (Ast.IdLid $meta_loc_bar loc$ "f"))
#     (Ast.ExId $meta_loc_bar loc$ (Ast.IdLid $meta_loc_bar loc$ "x")) >>
#         ... bar ...
#     end;
#   footer

camlp4 = '../boot/ocamlrun ./camlp4boot.run'
puts '(* Generated file, do not edit by hand! *)'
contents = ARGF.read.split("\n")
kinds  = []
header = []
footer = []
body   = []
cur    = header
for line in contents do
  if line =~ /<:start_meta<\s*(.*?)\s*>>/
    kinds = $1.split(/\s*,\s*/)
    cur = body
  elsif line =~ /<:stop_meta<.*>>/
    cur = footer
  elsif line =~ /(.*)<:meta<\s*(.*)\s*>>(.*)/
    pre, q, post = $1, $2, $3
    File.open('/tmp/metaq.ml', 'w') { |f| f.print q, ';' }
    antiquots = q.scan(/\$(?:(\w+):)?((\w+?)(\d*)(l)?)\$/)
    if antiquots.size == 1 and antiquots.first.first == 'anti'
      cur << "#{pre}#{q.gsub(/<:(\w+)</, '<:\1@_loc<').ljust(65)} -> <:meta_kind< $anti:s$ >>#{post}"
    else
      metaq = `#{camlp4} -printer OCaml -curry-constr /tmp/metaq.ml`.
        gsub(/;\s*\Z/, '').strip.gsub(/\b_?loc\b/, "$meta_loc_meta_kind _loc$")
      abort "abort: #{q}" unless $? == 0
      for antiquot in antiquots do
        name, lvar, var, off, list = antiquot
        case name
        when /^(chr|flo|(native)?int(32|64)?|lid|uid|anti)$/
          res = "meta_s _loc #{lvar}"
          # res = "meta_s #{lvar}"
        when /^(exp|pat|typ|mtyp|mexp|sigi|stri|id|tup)$/
          res = "meta_#{var}#{list} #{lvar}"
        when /^(opt|list|anti|when)$/
          if name =~ /^(opt|when)$/
            if var =~ /^(b|pr|m|v|r)$/
              res = "meta_bool _loc #{lvar}"
            # elsif var == 's'
            else
              res = "meta_opt _loc meta_#{var} #{lvar}"
              # res = "meta_#{var} #{lvar}"
            end
          else
            res = "meta_#{name} _loc meta_#{var} #{lvar}"
          end
        when /^(to|str)$/
          res = "meta_#{name} _loc #{lvar}"
        when String
          abort "abort: #{q}"
        else
          if var == 's'
            res = "meta_#{var}#{list} _loc #{lvar}"
          else
            res = "meta_#{var}#{list} #{lvar}"
          end
        end
        metaq.gsub!(/\b#{lvar}\b/, "$#{res}$")
      end
      cur << "#{pre}#{q.gsub(/<:(\w+)</, '<:\1@_loc<').ljust(65)} -> <:meta_kind< #{metaq} >>#{post}"
    end
  else
    cur << line
  end
end
body = body.join("\n  ")
puts header.join("\n")
for kind in kinds do
  puts
  puts "  module #{kind.capitalize} = struct"
  puts body.gsub(/meta_kind/, kind)
  puts "  end;"
end
puts footer.join("\n")
