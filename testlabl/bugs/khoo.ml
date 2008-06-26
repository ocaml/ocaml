class ['a] c (a : 'a) =
    object (s)
        method s = s
        method d : int = match a with `A b -> b#num
    end

let create x = new c x
