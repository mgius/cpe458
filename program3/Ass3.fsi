module Ass3

type term = (double * double)
type poly = term list

val findZeros : poly * double -> double

type event1 = bool
type event = int -> event1
type rv = event -> double

val expectedVal : rv -> double -> double
