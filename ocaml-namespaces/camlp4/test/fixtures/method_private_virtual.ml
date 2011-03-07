class virtual c1 = object method virtual private f : unit end;;
class virtual c2 = object method private virtual f : unit end;;

<:str_item< class virtual c1 = object method virtual private f : unit; end >>;;
<:str_item< class virtual c2 = object method private virtual f : unit; end >>;;
<:str_item< class virtual c2 = object method $private:p$ virtual f : unit; end >>;;
<:str_item< class virtual c2 = object method virtual $private:p$ f : unit; end >>;;
<:str_item< class $virtual:v$ c2 [$t1$] =
              object ($pat:self$) method virtual $private:p$ $lid:f$ : $t2$; end >>;;
