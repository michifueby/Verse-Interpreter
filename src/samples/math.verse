
solveTowersOfHanoi(n:int, src:string, aux:string, dst:string) := if (n = 1) then "Move disk 1 from " + src + " to " + dst else { solveTowersOfHanoi(n - 1, src, dst, aux); "Move disk " + n + " from " + src + " to " + dst; solveTowersOfHanoi(n - 1, aux, src, dst);}

n := 3
src := "A"
aux := "B"
dst := "C"

solveTowersOfHanoi(n, src, aux, dst)
