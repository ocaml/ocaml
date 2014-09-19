let f x = if x < 0 then x else h (x-1)
and g x = if x < 0 then x else f (x-1)
and h x = if x < 0 then x else g (x-1) 
