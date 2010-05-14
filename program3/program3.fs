type term = (double * double)
type eqn = term list

let deriv difference func x = 
   func (x + differ) - func x

let nr equation initial = 
   
