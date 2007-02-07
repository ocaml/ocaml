# tolower.sed expands one ...<:lower<FOO>>... to ...foo... per line
h
s/.*<:lower<\(.*\)>>.*/\1/
t cont
b end
:cont
y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/
s/$/|/
G
s/\(.*\)|\n\(.*\)<:lower<\(.*\)>>/\2\1/
:end
