
while (u <= n - 1 and v <= m - 1) {
  if (aDemand[ v] - aS[ v] < aSupply[ u] - aD[ u]) {
  z  = aDemand[ v] - aS[ v]
aRoute[ u][ v] = z
aS[ v]  += z
aD[ u]+= z
v += 1
else {
  z              = aSupply[ u] - aD[ u]
aRoute[ u][ v] = z
aS[ v]+= z
aD[ u]  += z
u += 1 }

function(~~NotOptimal() {
  global PivotN
global PivotM
nMax = -nVeryLargeNumber
GetDual()
for u in range( 0, n):
  for v in range( 0, m):
  x = aDual[ u][ v]
if x > nMax:
  nMax = x
PivotN = u
PivotM = v
return ( nMax > 0)
})

function(~ GetDual(){
  global aDual
for (u in range( 0, n))
  for (v in range( 0, m))
  aDual[ u][ v] = -0.5 
if aRoute[ u][ v] == 0:
  aPath = FindPath( u, v)
z     = -1
x     = 0
for (w in aPath) {
  x += z * aCost[ w[ 0]][ w[ 1]]
}
}
z *= -1
aDual[ u][ v] = x

function(~FindPath( u, v) aPath = [[ u, v]])

raw_input()
return aPath

function(~LookHorizontaly( aPath, u, v, u1, v1){
  for i in range( 0, m):
  if i != v and aRoute[ u][ i] != 0:
  if i == v1:
  aPath.append( [ u, i])
return True 
if (LookVerticaly( aPath, u, i, u1, v1))
  aPath.append( [ u, i])
return T
return F
}

 function(~LookVertical( aPath, u, v, u1, v1):
  for (i in range( 0, n)) {
  if i != u and aRoute[ i][ v] != 0:
  if (LookHorizontaly( aPath, i, v, u1, v1))
  aPath.append([ i, v])
  }
return T
return F)
def BetterOptimal():
  global aRoute
aPath = FindPath( PivotN, PivotM)
nMin  = nVeryLargeNumber
for (w in range( 1, len( aPath), 2)) {
  t = aRoute[ aPath[ w][ 0]][ aPath[ w][ 1]]
}
if (t < nMin) {
  nMin = t
}
for( w in range( 1 , len( aPath), 2)) {
  aRoute[ aPath[ w][ 0]][ aPath[ w][ 1]]  -= nMin
}
aRoute[ aPath[ w - 1][ 0]][ aPath[ w - 1][ 1]] += nMin

