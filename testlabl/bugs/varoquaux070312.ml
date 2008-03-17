type 'a exp=[
| `Add of 'e * 'e
| `Mul of 'e * 'e
]
constraint 'a = <exp:'e;..>

type 'a exp2=[
| 'a exp
| `Sub of 'e * 'e
| `Mul of 'e * 'e
]
constraint 'a = <exp2:'e;..>


type 'a b = unit
constraint 'a = int

and 'a c = 'a b
constraint 'a = unit
