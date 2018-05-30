qs(v) %::% numeric : numeric
qs(v) %as% if(length(v) > 1) c(qs(v[v < v[1]]), qs(v[v >= v[1]])) else v
qs(10:1)
