s/^# >>> IMPORTANT <<<     DO NOT MODIFY THIS LINE$/# >>> IMPORTANT <<< DO NOT MODIFY THIS FILE -- IT IS GENERATED FROM vaxKerN.s/
s/^\([^#"]*\)#/\1;/
/^\.set callee_save,~63$/s//	callee_save = ^C3/
{
: all_at
s/^\([^;"]*\)\*/\1@/
t all_at
}
{
: all_d
s/^\([^;"]*\)\$/\1#/
t all_d
}
s/^0x/^X/
{
: all_hex
s/^\([^;"]*[^0-9A-Za-z_;"]\)0x/\1^X/
t all_hex
}
{
: all_usB
s/^\([^;"]*\)_B/\1B/
t all_usB
}
s/\.data[ 	][ 	]*;\(.*\)$/.psect	\1,noexe,quad/
s/\.text[ 	][ 	]*;\(.*\)$/.psect	\1,exe,shr,pic,nowrt,quad/
$a\
		.end
