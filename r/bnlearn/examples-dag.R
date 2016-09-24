install.packages('bnlearn')
library('bnlearn')
e = empty.graph(LETTERS[1:6])
e
class(e)
empty.graph(LETTERS[1:6], num = 2)
arc.set = matrix(c("A", "C", "B", "F", "C", "F"),
                 ncol = 2, byrow = TRUE,
                 dimnames = list(NULL, c("from", "to")))
arc.set
arcs(e) = arc.set
e
bogus = matrix(c("X", "Y", "W", "Z"), 
               byrow = TRUE, ncol=2,
               dimnames = list(NULL, c("from", "to")))
bogus
arcs(e) = bogus
adj = matrix(0L, nrow=6, ncol=6, dimnames=list(LETTERS[1:6],LETTERS[1:6]))
adj
adj["A", "C"] = 1L
adj["B", "F"] = 1L
adj["C", "F"] = 1L
adj["D", "E"] = 1L
adj["A", "E"] = 1L
adj
amat(e) = adj
e
model2network("[A][C][B|A][D|C][F|A:B:D][E|F]")
modelstring(e) = "[A][C][B|A][D|C][F|A:B:C][E|F]"
e

random.graph(LETTERS[1:6], prob=0.1)
random.graph(LETTERS[1:6], num = 2, method='ic-dag')